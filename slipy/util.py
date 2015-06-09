import os


# http://python-forum.org/viewtopic.php?t=6185&p=8031
def raw_input(prompt, c=31366):
    os.write(1, prompt)
    res = os.read(0, c)
    return res.rstrip('\n')


def write(text):
    os.write(1, text)


# RPython has no zip, unfortunately
# TODO: better performance?
def zip(l1, l2):
    if not l1:
        return []
    if not l2:
        return []
    return [(l1[0], l2[0])] + zip(l1[1:], l2[1:])