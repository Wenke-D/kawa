n = 65


def print_num(c):
    print("putchar(%d);" % ord(c))

def print_str_num(s):
    for l in s:
        print_num(l)

print_str_num("Hello world!\n")