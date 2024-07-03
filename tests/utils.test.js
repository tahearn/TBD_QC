import { expect, test } from 'vitest';
import { getRangeValues } from '../lib/utils';


test('getRangeValues(1) returns [1]', () => {
  expect(getRangeValues(1)).toStrictEqual([1])
});

test(`getRangeValues('1') returns [1]`, () => {
  expect(getRangeValues('1')).toStrictEqual([1])
});

test(`getRangeValues('1,2,3') returns [1, 2, 3]`, () => {
  expect(getRangeValues('1,2,3')).toStrictEqual([1, 2, 3])
});

test(`getRangeValues(['1', '2', '3']) returns [1, 2, 3]`, () => {
  expect(getRangeValues(['1', '2', '3'])).toStrictEqual([1, 2, 3])
});

test(`getRangeValues('a,b,c') returns ['a', 'b', 'c']`, () => {
  expect(getRangeValues('a,b,c')).toStrictEqual(['a', 'b', 'c'])
});

test(`getRangeValues([1, 2, 3]) returns [1, 2, 3]`, () => {
  expect(getRangeValues([1, 2, 3])).toStrictEqual([1, 2, 3])
});

test(`getRangeValues('1.5, 2.7, 3.2') returns [1.5, 2.7, 3.2]`, () => {
  expect(getRangeValues('1.5, 2.7, 3.2')).toStrictEqual([1.5, 2.7, 3.2])
});

test(`getRangeValues([1, 2, 'c']) returns ['1', '2', 'c']`, () => {
  expect(getRangeValues([1, 2, 'c'])).toStrictEqual(['1', '2', 'c'])
});

test(`getRangeValues([1, 2, '3']) returns [1, 2, 3]`, () => {
  expect(getRangeValues([1, 2, '3'])).toStrictEqual([1, 2, 3])
});
