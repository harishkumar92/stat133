"""
This is lab 4.  Please do *not* remove any of the docstrings
from this document.  Docstrings are an important part of a
Python program:

    A docstring is a string literal that occurs as the first
    statement in a module, function, class, or method definition.
    Such a docstring becomes the __doc__ special attribute of
    that object.

    All modules should normally have docstrings, and all
    functions and classes exported by a module should also
    have docstrings. Public methods (including the __init__
    constructor) should also have docstrings.

    --- http://legacy.python.org/dev/peps/pep-0257/

Docstrings also allow for the use of doctests.

    The doctest module searches for pieces of text that look
    like interactive Python sessions, and then executes those
    sessions to verify that they work exactly as shown.

    --- http://docs.python.org/2/library/doctest.html

In this lab, you will create a Python class for creating
simple substitution ciphers.   You are free to implement
the required functionality however you would like, but
I highly recommend using Python's string maketrans()
method.

Imagine that you want to replace all the lower case
letters of the alphabet with the corresponding letter
of the reverse alphabet (i.e, you want to replace
a with z, b with y, c with x, d with w, and so on.)

First let's create a string of the lower case letters:

>>> alphabet = "abcdefghijklmnopqrstuvwxyz"

Now let's reverse the alphabet:

>>> alphabet[::-1]
'zyxwvutsrqponmlkjihgfedcba'
>>> key = alphabet[::-1]

You can now make a little translation table using
maketrans by passing it a string containing the
characters you want to replace and anoter string with
the characters you them to be replaced by:

>>> import string
>>> translator = string.maketrans(alphabet, key)

Note that you can give it any characters you want
not just letters.  But the two strings that you
call maketrans with must be the same length.

Now you can call the translate method of any string
with the translation table and it will return the
translated message.

>>> msg = "hello"
>>> msg.translate(translator)
'svool'
>>> coded = msg.translate(translator)
>>> decoded = coded.translate(translator)
>>> decoded
'hello'
"""

import string

class Cipher(object):
    """
    >>> reverse = Cipher("reverse")
    >>> reverse.encrypt("hello")
    'svool'
    >>> reverse.decrypt("svool")
    'hello'
    >>> rot13 = Cipher("rot13")
    >>> rot13.encrypt("hello")
    'uryyb'
    >>> rot13.encrypt("abcdefghijklmnopqrstuvwxyz")
    'nopqrstuvwxyzabcdefghijklm'
    >>> rot13.isvalid("hello")
    True
    >>> rot13.isvalid("H3aff")
    False
    >>> hybrid = Cipher("hybrid")
    >>> hybrid.encrypt("abcdefghijklmnopqrstuvwxyz")
    'nmlkjihgfedcbazyxwvutsrqpo'
    """

    def __init__(self, mapping):
        """
        The constructor method.
        """
        alphabet = "abcdefghijklmnopqrstuvwxyz"
        if mapping =="reverse":
            key = alphabet[::-1]
            self.translator = string.maketrans(alphabet, key)
            self.decryptor = string.maketrans(key, alphabet)
        elif mapping == "rot13":
            key = alphabet[-13:] + alphabet[:len(alphabet) - 13]
            self.translator = string.maketrans(alphabet, key)
            self.decryptor = string.maketrans(key, alphabet)
        elif mapping == "hybrid":
            key = alphabet[:14][::-1] + alphabet[14:][::-1]
            self.translator = string.maketrans(alphabet, key)
            self.decryptor = string.maketrans(key, alphabet)


    def isvalid(self, message):
        """
        Checks whether a string is a valid for this class.

        Parameters
        ----------
        message : string
            A string (i.e., you can assume you get a string)

        Returns
        -------
        out : bool
            Returns True, if message is only composed of letters
            in our alphabet (and False otherwise).
        """
        return (message.isalpha() and message.islower())

    def encrypt(self, message):
        """
        Encrypt the message

        Parameters
        ----------
        message: string
            A string (i.e., you can assume you get a string)

        Returns
        -------
        out : string
        """
        return message.translate(self.translator)

    def decrypt(self, message):
        """
        Decrypt the message

        Parameters
        ----------
        message: string
            A string (i.e., you can assume you get a string)

        Returns
        -------
        out : string
        """
        return message.translate(self.decryptor)

if __name__ == "__main__":
    import doctest
    doctest.testmod()
"""
This is lab 4.  Please do *not* remove any of the docstrings
from this document.  Docstrings are an important part of a
Python program:

    A docstring is a string literal that occurs as the first
    statement in a module, function, class, or method definition.
    Such a docstring becomes the __doc__ special attribute of
    that object.

    All modules should normally have docstrings, and all
    functions and classes exported by a module should also
    have docstrings. Public methods (including the __init__
    constructor) should also have docstrings.

    --- http://legacy.python.org/dev/peps/pep-0257/

Docstrings also allow for the use of doctests.

    The doctest module searches for pieces of text that look
    like interactive Python sessions, and then executes those
    sessions to verify that they work exactly as shown.

    --- http://docs.python.org/2/library/doctest.html

In this lab, you will create a Python class for creating
simple substitution ciphers.   You are free to implement
the required functionality however you would like, but
I highly recommend using Python's string maketrans()
method.

Imagine that you want to replace all the lower case
letters of the alphabet with the corresponding letter
of the reverse alphabet (i.e, you want to replace
a with z, b with y, c with x, d with w, and so on.)

First let's create a string of the lower case letters:

>>> alphabet = "abcdefghijklmnopqrstuvwxyz"

Now let's reverse the alphabet:

>>> alphabet[::-1]
'zyxwvutsrqponmlkjihgfedcba'
>>> key = alphabet[::-1]

You can now make a little translation table using
maketrans by passing it a string containing the
characters you want to replace and anoter string with
the characters you them to be replaced by:

>>> import string
>>> translator = string.maketrans(alphabet, key)

Note that you can give it any characters you want
not just letters.  But the two strings that you
call maketrans with must be the same length.

Now you can call the translate method of any string
with the translation table and it will return the
translated message.

>>> msg = "hello"
>>> msg.translate(translator)
'svool'
>>> coded = msg.translate(translator)
>>> decoded = coded.translate(translator)
>>> decoded
'hello'
"""

import string

class Cipher(object):
    """
    >>> reverse = Cipher("reverse")
    >>> reverse.encrypt("hello")
    'svool'
    >>> reverse.decrypt("svool")
    'hello'
    >>> rot13 = Cipher("rot13")
    >>> rot13.encrypt("hello")
    'uryyb'
    >>> rot13.encrypt("abcdefghijklmnopqrstuvwxyz")
    'nopqrstuvwxyzabcdefghijklm'
    >>> rot13.isvalid("hello")
    True
    >>> rot13.isvalid("H3aff")
    False
    >>> hybrid = Cipher("hybrid")
    >>> hybrid.encrypt("abcdefghijklmnopqrstuvwxyz")
    'nmlkjihgfedcbazyxwvutsrqpo'
    """

    def __init__(self, mapping):
        """
        The constructor method.
        """
        alphabet = "abcdefghijklmnopqrstuvwxyz"
        if mapping =="reverse":
            key = alphabet[::-1]
            self.translator = string.maketrans(alphabet, key)
            self.decryptor = string.maketrans(key, alphabet)
        elif mapping == "rot13":
            key = alphabet[-13:] + alphabet[:len(alphabet) - 13]
            self.translator = string.maketrans(alphabet, key)
            self.decryptor = string.maketrans(key, alphabet)
        elif mapping == "hybrid":
            key = alphabet[:14][::-1] + alphabet[14:][::-1]
            self.translator = string.maketrans(alphabet, key)
            self.decryptor = string.maketrans(key, alphabet)


    def isvalid(self, message):
        """
        Checks whether a string is a valid for this class.

        Parameters
        ----------
        message : string
            A string (i.e., you can assume you get a string)

        Returns
        -------
        out : bool
            Returns True, if message is only composed of letters
            in our alphabet (and False otherwise).
        """
        return (message.isalpha() and message.islower())

    def encrypt(self, message):
        """
        Encrypt the message

        Parameters
        ----------
        message: string
            A string (i.e., you can assume you get a string)

        Returns
        -------
        out : string
        """
        return message.translate(self.translator)

    def decrypt(self, message):
        """
        Decrypt the message

        Parameters
        ----------
        message: string
            A string (i.e., you can assume you get a string)

        Returns
        -------
        out : string
        """
        return message.translate(self.decryptor)

if __name__ == "__main__":
    import doctest
    doctest.testmod()
