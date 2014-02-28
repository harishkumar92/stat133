from numpy import array,zeros
from numpy.random import rand,randint
from pandas import DataFrame,Series


def simulate_grades(class_size, max_scores=[100,100,100]):
    """
    Simulate student grades of class_size students for graded_items 
    Rows correspond to students
    Columns correspond to grade item (such final, midterm, or homework)

    Parameters
    ----------
    class_size : int 
        Number of student grade reccords to simulate
    
    max_scores: list 
	list of the maximal possible scores

    Returns
    -------
    out : numpy array
	Array with class_size rows and len(graded_item) columns and
	column i values randomly generated numbers 
	between 0 and max_scores[i]

    Hint
    ----

    Examples
    --------
    >>> simulate_grades(4,[5,5,5]).shape == (4,3)
    True
    >>> simulate_grades(250, [100, 100, 100]).max() < 100
    True
    >>> simulate_grades(250, [100, 100, 100]).min() > 0
    True
    """
    columns = len(max_scores)
    shape = (class_size, columns)
    returnVal = zeros(shape)
    
    
    for row in range(0, shape[0]):
        for column in range(0,shape[1]):
            returnVal[row,column] = rand()*max_scores[column]
    return returnVal



def simulate_grade_df(class_size, grade_items={'F':100,'M':100,'HW':10}):
    """
    Simulate a Pandas DataFrame of grades 

    Parameters
    ----------
    class_size : int 
        Number of student grade reccords to simulates
    
    graded_items: dict 
        Dictionary keys are the graded item names
	Dictionary values are the maximal possible scores

    Returns
    -------
    out : Pandas DataFrame 
	DataFrame with class_size rows and len(graded_item) columns
	The rows correspond to students
	The columns must be labelled with the keys of grade_items
	The values are the same as generated by the previous function

    Hint
    ----
    Reuse the function simulate_grades in this function body

    Examples
    --------
    >>> simulate_grade_df(4,{'M':5,'F':5,'HW':5}).shape == (4,3)
    True
    """
    max_scores = grade_items.values()
    exams = grade_items.keys()
    
    numpy_grades = simulate_grades(class_size, max_scores)
    return DataFrame(numpy_grades, columns=exams)



class GradeBook(object):
    """A class encapsulating a pandas DataFrame and meant to store 
    the grades for a whole class. It provides the method compute_total_grades
    that compute the totla grade for each student according to a weights provided
    by the caller.
    """

    def __init__(self, grade_arr, student_ids, item_list, max_scores):
        """
        Constructor of the class grade frame: 
	It should set the following attributes:

	(1) self.raw_grades, which is a DataFrame with 
	        - row labels given by student_ids
	        - column labels given by item_list        for column in range(0,shape[1]):
            print max_scores[column]
	        - values given by grade_arr

        (2) self.total_grades, set to None

	(3) self.letter_grades, set to None

	(4) self.max_scores, set to max_scores
        
        Parameters
        ----------
        grade_arr : numpy array of grades as returned by simulate_grades

        student_ids: a list of student ids 

	item_list: a list of grade items (e.g. ['HW', 'M', 'F'])

	max_scores: a list of the maximum possible score for each grade item
        
        Returns
        -------
        nothing 
        
        Examples
        --------
        >>> a = GradeBook(array([[1,2],[3,4]]),['22','34'],['F','M'],[30, 50])
        >>> a.letter_grades == None
        True
        >>> a.total_grades == None
        True
        >>> a.raw_grades.shape == (2,2)
        True
        >>> a.raw_grades.ix[0,0] == 1
        True
	>>> a.max_scores[0] == 30
	True
        """
        self.raw_grades = DataFrame(grade_arr,index=student_ids, columns=item_list)
        self.total_grades = None
        self.letter_grades = None
        self.max_scores = max_scores
        


    def compute_total_grades(self, item_weights=None, max_score=100):
        """
        Compute student total class grades as a weighted average of the column in self.raw_grades 
        according to the weights passed to item_weight for each of the columns.
        The student total class grades are then stored in the Series attribute self.total_grades
        The return value should be a Series containing a numerical summary
        (as returned by the Series method describe) of the total class grade distribution. 
    
        Parameters
        ----------
        item_weights: list of floats summing up to one
            List of weights to be applied to each grade item (e.g. [0.3, 0.4, 0.3]) 
        
        max_score: float 
            Maximal possible score for the total class grade	
    
        Returns
        -------
        out : Series 
            A Series containing a numerical summary of the total 
    	grade distribution previously stored by the function 
    	in the attribute self.total_grades; this Series is the
    	output of the Series method describe.
        ----
    
        Examples
        --------
        >>> a = GradeBook(array([[5,5],[1,1]]),['22','34'],['F','M'],[10, 10])
	>>> b = a.compute_total_grades([0.5, 0.5], 100)
	>>> len(b) == 5
	False
	>>> a.total_grades['22'] == 50
	True
	>>> a.total_grades['34'] == 10
	True
        """
        
	return NotImplemented
         




if __name__ == "__main__":
    import doctest
    doctest.testmod()
