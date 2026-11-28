# TLS test certificates

Test-only keys and certificates for the TLS tests. Never use them for anything else: the private keys are public.

| File | What it is |
|---|---|
| `ca.pem`, `ca.key` | The test CA ("Dusk Test CA") that signs the others |
| `server.pem`, `server.key` | A server certificate for `localhost`, `127.0.0.1` and `::1`, signed by the test CA |
| `expired.pem` | The same server, valid only during 2020 (key: `server.key`) |
| `other-host.pem` | A certificate for `other.example` only (key: `server.key`) |
| `self-signed.pem`, `self-signed.key` | A self-signed certificate for `localhost` and `127.0.0.1`, not signed by the test CA |

All keys are P-256. Everything except `expired.pem` is valid for 100 years from 2026-11-15. They were made with OpenSSL 3.5 (`-not_before` / `-not_after` need 3.4 or newer):

```sh
openssl ecparam -name prime256v1 -genkey -noout -out ca.key
openssl req -x509 -new -key ca.key -sha256 -days 36500 -subj "/CN=Dusk Test CA" -out ca.pem

openssl ecparam -name prime256v1 -genkey -noout -out server.key
openssl req -new -key server.key -subj "/CN=localhost" -out server.csr
printf "subjectAltName=DNS:localhost,IP:127.0.0.1,IP:::1\nbasicConstraints=CA:FALSE\nkeyUsage=digitalSignature\nextendedKeyUsage=serverAuth\n" > server.ext
openssl x509 -req -in server.csr -CA ca.pem -CAkey ca.key -set_serial 2 -days 36500 -sha256 -extfile server.ext -out server.pem
openssl x509 -req -in server.csr -CA ca.pem -CAkey ca.key -set_serial 3 -not_before 20200101000000Z -not_after 20210101000000Z -sha256 -extfile server.ext -out expired.pem

printf "subjectAltName=DNS:other.example\nbasicConstraints=CA:FALSE\nkeyUsage=digitalSignature\nextendedKeyUsage=serverAuth\n" > other.ext
openssl x509 -req -in server.csr -CA ca.pem -CAkey ca.key -set_serial 4 -days 36500 -sha256 -extfile other.ext -out other-host.pem

openssl ecparam -name prime256v1 -genkey -noout -out self-signed.key
openssl req -x509 -new -key self-signed.key -sha256 -days 36500 -subj "/CN=localhost" -addext "subjectAltName=DNS:localhost,IP:127.0.0.1" -out self-signed.pem

rm server.csr server.ext other.ext
```
