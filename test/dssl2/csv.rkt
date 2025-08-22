#lang dssl2

import csv

# looks files up with a relative path; awkward to test
# run from the current directory to test
#assert read_csv('test.csv') == [['1','2','3'],['2','3','4'],['5','6','7']]
#assert read_csv_headers('test.csv') == ['1','2','3']
#assert read_csv_data('test.csv') == [['2','3','4'],['5','6','7']]

assert_error read_csv('does_not_exist')
assert_error read_csv('/bin/bash')
