#lang dssl2

let bin_arr = [
    [237, 50, 25, 90, 255],
    [10, 23],   # 26 is \n
    [18, 2, 0, 47, 198, 97],
    [99],
    [],
    [58, 13],   # 13 is \r
    [90, 75, 121, 16, 207, 58]
]

# write text: write()
let wt = open('./test_resources/text.txt', 'w', 't')
assert_eq wt.write('this is sorta fun!!\n'), 20
assert_eq wt.write('\ni am making a DSSL2 test!!'), 27
assert_eq wt.write('\n\n'), 2
assert_eq wt.write('ok...'), 5
assert_eq wt.write('i will be done now'), 18
assert_error wt.readline(), 'readline: file must be in \'r\' mode'
assert_error wt.readchar(), 'readchar: file must be in \'r\' mode'
wt.close()

# read text: readchar(), readline()
let rt = open('./test_resources/text.txt', 'r', 't')
assert_eq rt.readchar(), 't'
assert_eq rt.readchar(), 'h'
assert_eq rt.readline(), 'is is sorta fun!!'
assert_eq rt.readline(), ''
assert_eq rt.readline(), 'i am making a DSSL2 test!!'
assert_eq rt.readchar(), '\n'
assert_eq rt.readline(), 'ok...i will be done now'
assert_eq rt.readline(), EOF
assert_eq rt.readchar(), EOF
assert_error rt.write('pls'), 'write: file must be in \'w\' mode'
rt.close()

# write binary: write()
let wb = open('./test_resources/bin.txt', 'w', 'b')
assert_eq wb.write(bin_arr[0]), bin_arr[0].len()
assert_eq wb.write(bin_arr[1]), bin_arr[1].len()
assert_eq wb.write(bin_arr[2]), bin_arr[2].len()
assert_eq wb.write(bin_arr[3]), bin_arr[3].len()
assert_eq wb.write(bin_arr[4]), bin_arr[4].len()
assert_eq wb.write(bin_arr[5]), bin_arr[5].len()
assert_eq wb.write(bin_arr[6]), bin_arr[6].len()
assert_error wb.write(23), 'write: invalid argument'
wb.close()

# read binary: readbyte(), readline()
let rb = open('./test_resources/bin.txt', 'r', 'b')
assert_eq rb.readbyte(), 237
assert_eq rb.readbyte(), 50
assert_eq rb.readbyte(), 25
assert_eq rb.readline(), [90, 255]
assert_eq rb.readline(), [23, 18, 2, 0, 47, 198, 97, 99, 58]
assert_eq rb.readbyte(), 90
assert_eq rb.readbyte(), 75
assert_eq rb.readline(), [121, 16, 207, 58]
assert_eq rb.readline(), EOF
assert_eq rb.readbyte(), EOF
rb.close()
