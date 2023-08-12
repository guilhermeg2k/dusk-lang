import { expect, test } from 'bun:test';
import { Lexer } from './main';

test('test all tokens', () => {
  let input = `let mut if else match for type trait where
use mod return true false or and
\t= == != < > <= >= + - * / ! # ### * ( ) { } [ ] . , : =>
\t... salve salve_mundo 123 salve2mundo _salve_mundo
\0`;

  console.log(input);

  const lexer = new Lexer(input);
  console.log(lexer.tokens);
  console.log(JSON.stringify(lexer.tokens));
});
