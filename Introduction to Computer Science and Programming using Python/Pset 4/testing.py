#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import string, random

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

def build_transpose_dict(vowels_permutation):
        '''
        vowels_permutation (string): a string containing a permutation of vowels (a, e, i, o, u)
        
        Creates a dictionary that can be used to apply a cipher to a letter.
        The dictionary maps every uppercase and lowercase letter to an
        uppercase and lowercase letter, respectively. Vowels are shuffled 
        according to vowels_permutation. The first letter in vowels_permutation 
        corresponds to a, the second to e, and so on in the order a, e, i, o, u.
        The consonants remain the same. The dictionary should have 52 
        keys of all the uppercase letters and all the lowercase letters.

        Example: When input "eaiuo":
        Mapping is a->e, e->a, i->i, o->u, u->o
        and "Hello World!" maps to "Hallu Wurld!"

        Returns: a dictionary mapping a letter (string) to 
                 another letter (string). 
        '''
        vowel_map = {'a':0,'e':1,'i':2,'o':3,'u':4,'A':0,'E':1,'I':2,'O':3,'U':4}
        
        lower_alpha = string.ascii_lowercase
        upper_alpha = string.ascii_uppercase
        lower_vowels = 'aeiou'
        upper_vowels = 'AEIOU'
        
        lower_dict = {}
        upper_dict = {}
        
        lower_vowel_permutations = get_permutations(lower_vowels)
        upper_vowel_permutations = get_permutations(upper_vowels)
        
        lower_vowel_shift = random.choice(lower_vowel_permutations)
        upper_vowel_shift = random.choice(upper_vowel_permutations)
        
        lower_shift_dict = {}
        reverse_lower_shift_dict = {}
        upper_shift_dict = {}
        reverse_upper_shift_dict = {}
        
        for count, letter in enumerate(lower_vowel_shift):
            lower_shift_dict[letter] = count
        
        for count, letter in enumerate(upper_vowel_shift):
            upper_shift_dict[letter] = count

        for letter in lower_alpha:
            if letter in lower_vowels:
                lower_dict[letter] = vowel_map[letter]
                continue
            lower_dict[letter] = letter
            
        for letter in upper_alpha:
            if letter in upper_vowels:
                upper_dict[letter] = vowel_map[letter]
                continue
            upper_dict[letter] = letter
        
        for k,v in lower_shift_dict.items():
            reverse_lower_shift_dict[v] = k
            
        for k,v in upper_shift_dict.items():
            reverse_upper_shift_dict[v] = k
            
        for key in lower_dict.keys():
            if key not in lower_vowels:
                continue
            for vowel in lower_shift_dict:
                if lower_dict[key] == lower_shift_dict[vowel]:
                    lower_dict[key] = reverse_lower_shift_dict[lower_dict[key]]
        
        for key in upper_dict.keys():
            if key not in upper_vowels:
                continue
            for vowel in upper_shift_dict:
                if upper_dict[key] == upper_shift_dict[vowel]:
                    upper_dict[key] = reverse_upper_shift_dict[upper_dict[key]]    
            
        shifted_dict = dict(**lower_dict,**upper_dict)
        
        return shifted_dict
    
def apply_transpose(text,transpose_dict):
        '''
        transpose_dict (dict): a transpose dictionary
        
        Returns: an encrypted version of the message text, based 
        on the dictionary
        '''
        transposed_message = ''
        
        #if letter in text, return the transposed version
        for letter in text:
            transposed_message += transpose_dict[letter]
        
        return transposed_message
            
        
print(apply_transpose('hello',build_transpose_dict('aeiou')))