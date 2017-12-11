from misc import Failure

class Vector(object):
    def __init__(self, args):
        """ this is the ctor for the Vector class, it takes in 
        takes in one arg and if the arg is a int or long it
        checks to make sure that the value is positive.
        if it is a list it uses that list as the values and 
        length of the vector. It raises an error on other inputs"""    
        if isinstance(args, int) or isinstance(args,long):
            if (args < 0):
                raise ValueError("Vector length cannot be negative")
            self.values = [0.0] * args 
            self.len = args
        elif isinstance(args, list):
            self.values = list(args)    
            self.len = len(args)
        else:
           raise TypeError("Please input a number or a list")
           
    def __repr__(self):
        """ repr is the string represention of the class, it returns
        Vector(contents of the list)"""
        return "Vector(" + repr(self.values) + ")"

    def __len__(self):
        """ len returns the length of the vector"""
        return len(self.values)

    def __iter__(self):
        """ iter returns an object that can iterate over the values of the
        vector. it uses yield to iterate"""
        for x in self.values:
       		yield x

    def __add__(self, other):
       """ add is used to add the elements of a vector to a sequence
       to return another vector, zip is used to bind elemnents"""

    	return Vector([x + y for x, y in zip(self.values, list(other))])
