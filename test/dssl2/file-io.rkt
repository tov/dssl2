#lang dssl2

let FILE1 = 'test1.out'
let FILE2 = 'test2.out'

let MESSAGE1 = 'this is sorta fun!!\n'
let MESSAGE2 = 'this is a different string, tho'

assert_error file_to_string(FILE1)

string_to_file(MESSAGE1, FILE1)
assert file_to_string(FILE1) == MESSAGE1

string_to_file(MESSAGE2, FILE1)
assert file_to_string(FILE1) == MESSAGE2

assert_error file_to_string(FILE2)
