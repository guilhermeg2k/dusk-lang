#include <stddef.h>
#ifdef _WIN32
#ifndef _WIN32_WINNT
#define _WIN32_WINNT 0x0602
#endif
#include <limits.h>
#include <windows.h>
#include <wchar.h>

static int are_transition_dates_equal(const SYSTEMTIME *left, const SYSTEMTIME *right) {
    if (left->wMonth != right->wMonth) {
        return 0;
    }
    if (left->wMonth == 0) {
        return 1;
    }
    return left->wYear == right->wYear && left->wDayOfWeek == right->wDayOfWeek && left->wDay == right->wDay && left->wHour == right->wHour && left->wMinute == right->wMinute && left->wSecond == right->wSecond && left->wMilliseconds == right->wMilliseconds;
}

int dusk_local_windows_timezone(char *name, size_t capacity) {
    if (name == NULL || capacity == 0 || capacity > INT_MAX) {
        return 0;
    }
    DYNAMIC_TIME_ZONE_INFORMATION current = {0};
    const DWORD timezone_status = GetDynamicTimeZoneInformation(&current);
    const int has_current_zone = timezone_status != TIME_ZONE_ID_INVALID && current.TimeZoneKeyName[0] != 0 && current.TimeZoneKeyName[127] == 0;
    if (!has_current_zone) {
        return 0;
    }
    SYSTEMTIME now;
    GetLocalTime(&now);
    for (DWORD index = 0; index < 4096; index++) {
        DYNAMIC_TIME_ZONE_INFORMATION known = {0};
        if (EnumDynamicTimeZoneInformation(index, &known) != ERROR_SUCCESS) {
            return 0;
        }
        if (known.TimeZoneKeyName[127] != 0) {
            return 0;
        }
        if (wcscmp(current.TimeZoneKeyName, known.TimeZoneKeyName) != 0) {
            continue;
        }
        TIME_ZONE_INFORMATION expected = {0};
        if (!GetTimeZoneInformationForYear(now.wYear, &known, &expected)) {
            return 0;
        }
        int has_matching_rules = current.Bias == expected.Bias && current.StandardBias == expected.StandardBias && current.DaylightBias == expected.DaylightBias && are_transition_dates_equal(&current.StandardDate, &expected.StandardDate) && are_transition_dates_equal(&current.DaylightDate, &expected.DaylightDate);
        if (!has_matching_rules) {
            return 0;
        }
        int is_plain_utc = wcscmp(current.TimeZoneKeyName, L"UTC") == 0 && current.Bias == 0 && current.StandardDate.wMonth == 0 && current.DaylightDate.wMonth == 0;
        if (current.DynamicDaylightTimeDisabled && !is_plain_utc) {
            return 0;
        }
        return WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS, current.TimeZoneKeyName, -1, name, (int)capacity, NULL, NULL) > 0;
    }
    return 0;
}
#else
int dusk_local_windows_timezone(char *name, size_t capacity) {
    (void)name;
    (void)capacity;
    return 0;
}
#endif
