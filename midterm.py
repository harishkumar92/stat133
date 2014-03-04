#---------------------------------------------------------------------------
#                    STAT 133 - MIDTERM - SPRING 2014
#---------------------------------------------------------------------------

"""
The exam has four questions equally weighted (10 points per question), and 
going in increasing order of difficulty.

For each question, you'll have to implement a function (or method) body, as 
detailed in the function's docstring. Each docstring contains 5 tests for 
your function; we will use very similar tests to grade your exam.

Make sure  that your function implementations pass the tests!

To see if your implementations pass the tests, type in the terminal: 

python midterm.py -v

If you are using the ipython notebook for coding help (for tab completion 
for instance), you can also run the tests from within the notebook by 
executing a code cell (ctrl + Enter) where you have typed:

%run midterm.py -v

Good luck with your exam!
"""


#---------------------------------------------------------------------------
# QUESTION 1: BOOLEAN EXPRESSIONS (10 points)
#---------------------------------------------------------------------------



def decision_rule(x, y, z):
    """
    QUESTION 1:
    --------- 
    Implement the function body so that it returns:
    - True when x and y are True, or when z is True
    - False when either x or y is False, and z is False

    TESTS 1:
    ---------
    >>> decision_rule(True, True, True)
    True
    >>> decision_rule(True, True, False)
    True
    >>> decision_rule(True, False, True)
    True
    >>> decision_rule(True, False, False)
    False
    >>> decision_rule(False, False, False)
    False
    """
    
     
    return NotImplemented 


#---------------------------------------------------------------------------
# QUESTION 2: STRINGS AND IF STATEMENTS (10 points)
#---------------------------------------------------------------------------


def compare_string_length(x, y):
    """
    QUESTION 2: 
    ---------
    Implement the function body so that it first converts 
    the arguments x and y to strings and then returns:
            1 whenever y is longer than x (as strings)
            0 whenever x and y have equal lengths (as strings)
           -1 whenever x is longer than y (as strings)
    
    TESTS 2:
    ---------
    >>> compare_string_length("b", "aa")
    1
    >>> compare_string_length("aa", "b")
    -1
    >>> compare_string_length("aaa", "bbb")
    0
    >>> compare_string_length(True, False)
    1
    >>> compare_string_length(True, 1)
    -1
    """

    return NotImplemented        


#---------------------------------------------------------------------------
# QUESTION 3 - NUMERICAL FUNCTIONS (10 points)
#---------------------------------------------------------------------------

def compute_grade(grades, k):
    """
    QUESTION 3:
    --------
    Implement this function body so that it returns the grade point average 
    from a list of grades after having dropped the lowest k grades, where k
    is an integer.

    TESTS 3:
    ---------
    >>> compute_grade([10, 1, 10], 0)
    7.0
    >>> compute_grade([10, 1, 10], 1)
    10.0
    >>> compute_grade([10, 1, 10], 2)
    10.0
    >>> compute_grade([10], 0)
    10.0
    >>> compute_grade([1,2,3,4], 3)
    4.0
    """
    
    return NotImplemented
    

#---------------------------------------------------------------------------
# QUESTION 4 - CLASSES (10 points)
#---------------------------------------------------------------------------

class DNA(object):
    """
    For this exercise, you will be implementing the method to_protein, which
    converts a DNA sequence to its corresponding protein. Specific instructions
    are given in the method docstring below. 
    
    Recall:
         A DNA sequence is a string composed of four nucleotides,
         represented by the uppercase letters 'A', 'C', 'G', and 'T'. 
         The length of the DNA sequence is always a multiple of 3 (for example, 
         'GCTAGG' or 'GAT', etc.).
         
         A DNA string can be broken into groups of 3 nucleotides each. These
         subgroups are called codons. For example, the DNA string 'GCTAGG' is 
         composed of codons 'GCT' and 'AGG'. 

         In turn, each codon corresponds to one of 21 amino acids, which are 
         represented by the following lowercase characters: "acedgfihkmlnqpsrtwvy-". 
         For instance, the codon 'AAA' corresponds to the amino acid 'k'; the 
         codon 'TGT' corresponds to the amino acid 'c'. Thus, the DNA string 
         'AAATGT' corresponds to the protein 'kc'. 

    """

    def to_protein(self):
        
        """
        QUESTION 4:
        ---------
        Implement this method body so that it:
        (1) computes the protein string corresponding to the DNA string 
            stored by the class constructor in the attribute self.dna;
        (2) stores this computed protein string into the attribute 
            self.protein; and
        (3) returns the attribute self.protein.
        
        Hint: Use the dictionary self.codon2amino (see the constructor below.)

        TESTS 4:
        --------
        >>> dna = DNA('ACT'); len(dna.to_protein()) == 1
        True
        >>> type(dna.to_protein()) == str
        True
        >>> dna.protein == dna.to_protein()
        True
        >>> set(dna.protein).issubset(dna.codon2amino.values())
        True
        >>> DNA('AGTCCACCCTAAACTCCACAG' * 4).to_protein()
        'spp-tpqspp-tpqspp-tpqspp-tpq'
        """
        
        return NotImplemented
        



    def __init__(self, dna):
        
        self.dna = dna
        
        self.codon2amino = {
        'TAT':'y', 'AAA':'k', 'TGT':'c', 'GGT':'g', 'TCT':'s', 'TAG':'-', 
        'TTT':'f', 'TGC':'c', 'TGA':'-', 'TGG':'w', 'TAC':'y', 'TTC':'f', 
        'TCG':'s', 'TTA':'l', 'TTG':'l', 'TCC':'s', 'TCA':'s', 'GCA':'a', 
        'GTA':'v', 'GCC':'a', 'GTC':'v', 'GCG':'a', 'GTG':'v', 'CGT':'r', 
        'GTT':'v', 'GCT':'a', 'ACC':'t', 'GAT':'d', 'CGA':'r', 'CGC':'r', 
        'ACT':'t', 'AAG':'k', 'CGG':'r', 'GGG':'g', 'GGA':'g', 'GGC':'g', 
        'GAG':'e', 'CAG':'q', 'GAC':'d', 'CAA':'q', 'GAA':'e', 'CTT':'l', 
        'ATG':'m', 'ACA':'t', 'ACG':'t', 'ATC':'i', 'AAC':'n', 'ATA':'i', 
        'AGG':'r', 'CCT':'p', 'AGC':'s', 'AGA':'r', 'CAT':'h', 'AAT':'n', 
        'ATT':'i', 'CTG':'l', 'CTA':'l', 'CTC':'l', 'CAC':'h', 'CCG':'p', 
        'AGT':'s', 'CCA':'p', 'CCC':'p', 'TAA':'-'}


    
    
#---------------------------------------------------------------------------
# DO NOT CHANGE WHAT'S BELOW
#---------------------------------------------------------------------------
if __name__ == "__main__":
    import doctest
    doctest.testmod()
