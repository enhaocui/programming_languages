from misc import Failure

class Vector(object):
    """Vector is a fixed length vector which implements a variety of operations."""
    def __init__(self, args):
        """Constructor of Vector. values: elements of the list, represented as a list. len: length of the list.
        Takes in an int or a list to construct Vector"""
        if isinstance(args, int) or isinstance(args,long):
            if args < 0:
                raise ValueError("Vector length cannot be negative")
            self.values = [0.0] * args 
            self.len = args
        elif isinstance(args, list):
            self.values = list(args)    
            self.len = len(args)
        else:
            raise TypeError("Please input a number or a list")

    def __repr__(self):
        """ repr returns the string represention of the class, it returns
        Vector(list)"""
        return "Vector(" + repr(self.values) + ")"

    def __len__(self):
        """len is the length of the vector"""
        return len(self.values)

    def __iter__(self):
        """iter returns an iterator that iterates over the values of the
        vector."""
        for x in self.values:
            yield x

    def __add__(self, other):
        """ add returns the result of Vector + Vector or Vector + list """
        return Vector([x + y for x, y in zip(self.values, list(other))])

    def __iadd__(self, other):
        """iadd is called for += operations"""
        self.values = [x + y for x, y in zip(list(self.values), list(other))]
        return self

    def __radd__(self, other):
        """radd is called when list + Vector"""
        return Vector([x + y for x, y in zip(list(other), self.values)])

    def dot(self, other):
        """dot returns the dot product for vector and vector or vector and list"""
        return sum(Vector([x * y for x, y in zip(self.values, list(other))]).values)

    def __getitem__(self, index):
        """getitem access the element of self.values at the input index"""
        if isinstance(index, slice): #[start, stop, step]
            return self.__getrange__(index)
        if index < 0:
            index += len(self.values)
        if index < 0 or index >= len(self.values):
            raise IndexError("Index out of range.")
        return self.values[index]

    def __getrange__(self, index):
        """getrange is called for range index access [start:end:step]"""
        start, end, step = index.indices(len(self))
        val = [self[i] for i in range(start, end, step)]
        return Vector(val)

    def __setitem__(self, index, val):
        """setitem sets the element at index with val, and should first check if the index is valid"""
        if isinstance(index, slice): #[start, stop, step]
            return self.__setrange__(index, val)
        if index < 0:
           index += len(self.values)
        if index < 0 or index >= len(self.values):
           raise IndexError("Index out of range.")
        self.values[index] = val

    def __setrange__(self, index, val):
        """setrange is called for range index set value [start:end:step]"""
        start, end, step = index.indices(len(self))
        j = 0
        for i in range(start, end, step):
            self[i] = val[j]
            j += 1

    def __getslice__(self, i, j):
        """getslice returns the slice of vector, [start::end], start included, end excluded"""
        if i < 0:
           i += len(self.values)
        if j < 0:
           j += len(self.values)
        if i < 0 or j < -1 or i >= len(self.values) or j > len(self.values):
           raise IndexError("Index out of range.")
        return self.values[i:j]

    def __setslice__(self, i, j, sequence):
        """setslice sets the slice of vector, [start::end], start included, end excluded"""
        if i < 0:
           i += len(self.values)
        if j < 0:
           j += len(self.values)
        if i < 0 or j < -1 or i >= len(self.values) or j > len(self.values):
           raise IndexError("Index out of range.")
        start = 0
        for ind in range(i, j):
           self.values[ind] = sequence[start]
           start += 1

    def __eq__(self, other):
        """eq checks if the two Vectors are equivalent. 
        They should be equivalent if they have the same elements WITH THE SAME ORDER"""
        if not isinstance(other, Vector):
            return False
        i = 0
        while i < len(self):
            if self[i] != other[i]:
                return False
            i += 1
        return True

    def __ne__(self, other):
        """ne checks if two vectors are not equivalent"""
        return not self == other

    def __gt__(self, other):
        """gr returns result for vector > vector, sort them first and then compare elements one by one"""
        self_sort = sorted(self, reverse = True)
        other_sort= sorted(other, reverse = True)
        for x, y in zip(self_sort, other_sort):
           if x > y:
              return True
           elif x < y:
              return False
        return False

    def __ge__(self, other):
        """ge returns result for vector >= vector, returns true if vector > vector, 
        or vector == vector AFTER THEY ARE SORTED"""
        if self > other:
            return True
        self_sort = sorted(self, reverse = True)
        other_sort= sorted(other, reverse = True)
        return self_sort == other_sort

    def __lt__(self, other):
        """lt returns vector < vector"""
        return not self >= other

    def __le__(self, other):
        """le returns vector <= vector"""
        return not self > other














