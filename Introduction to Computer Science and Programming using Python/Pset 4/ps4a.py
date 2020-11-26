# Problem Set 4A
# Name: <your name here>
# Collaborators:
# Time Spent: x:xx
def get_permutations(sequence):
    '''
    Enumerate all permutations of a given string

    sequence (string): an arbitrary string to permute. Assume that it is a
    non-empty string.  

    You MUST use recursion for this part. Non-recursive solutions will not be
    accepted.

    Returns: a list of all permutations of sequence

    Example:
    >>> get_permutations('abc')
    ['abc', 'acb', 'bac', 'bca', 'cab', 'cba']

    Note: depending on your implementation, you may return the permutations in
    a different order than what is listed here.
    '''
    #creates list of permutations
    perm_list = []
    
    #base case: if sequence is just one letter, return letter
    if len(sequence) == 1:
        return sequence
    else:
        #iterates through string, giving unique count to each element
        for count,letter in enumerate(sequence):
            
            #calls function but for the sequence minus the first letter
            #keeps on calling until length of sequence is equal to one
            for perm in get_permutations(sequence[:count] + sequence[count+1:]):
                
                #adds letter in front of all possible permutations without first letter
                perm_list += [letter + perm]
    return perm_list
    

if __name__ == '__main__':
#    #EXAMPLE
#    example_input = 'abc'
#    print('Input:', example_input)
#    print('Expected Output:', ['abc', 'acb', 'bac', 'bca', 'cab', 'cba'])
#    print('Actual Output:', get_permutations(example_input))
    
#    # Put three example test cases here (for your sanity, limit your inputs
#    to be three characters or fewer as you will have n! permutations for a 
#    sequence of length n)

     pass
print(get_permutations('aeiou'))
