# Problem Set 4C
# Name: <your name here>
# Collaborators:
# Time Spent: x:xx

import string
from ps4a import get_permutations

### HELPER CODE ###
def load_words(file_name):
    '''
    file_name (string): the name of the file containing 
    the list of words to load    
    
    Returns: a list of valid words. Words are strings of lowercase letters.
    
    Depending on the size of the word list, this function may
    take a while to finish.
    '''
    
    # inFile: file
    inFile = open(file_name, 'r')
    # wordlist: list of strings
    wordlist = []
    for line in inFile:
        wordlist.extend([word.lower() for word in line.split(' ')])
    return wordlist

def is_word(word_list, word):
    '''
    Determines if word is a valid word, ignoring
    capitalization and punctuation

    word_list (list): list of words in the dictionary.
    word (string): a possible word.
    
    Returns: True if word is in word_list, False otherwise

    Example:
    >>> is_word(word_list, 'bat') returns
    True
    >>> is_word(word_list, 'asdf') returns
    False
    '''
    word = word.lower()
    word = word.strip(" !@#$%^&*()-_+={}[]|\:;'<>?,./\"")
    return word in word_list

### END HELPER CODE ###

WORDLIST_FILENAME = 'words.txt'

# you may find these constants helpful
VOWELS_LOWER = 'aeiou'
VOWELS_UPPER = 'AEIOU'
CONSONANTS_LOWER = 'bcdfghjklmnpqrstvwxyz'
CONSONANTS_UPPER = 'BCDFGHJKLMNPQRSTVWXYZ'

class SubMessage(object):
    def __init__(self, text):
        '''
        Initializes a SubMessage object
                
        text (string): the message's text

        A SubMessage object has two attributes:
            self.message_text (string, determined by input text)
            self.valid_words (list, determined using helper function load_words)
        '''
        self.text = text
        self.valid_words = load_words(WORDLIST_FILENAME)
    
    def get_message_text(self):
        '''
        Used to safely access self.message_text outside of the class
        
        Returns: self.message_text
        '''
        return self.text

    def get_valid_words(self):
        '''
        Used to safely access a copy of self.valid_words outside of the class.
        This helps you avoid accidentally mutating class attributes.
        
        Returns: a COPY of self.valid_words
        '''
        return self.valid_words.copy()
                
    def build_transpose_dict(self, vowels_permutation):
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
        self.vowels_permutation = vowels_permutation
        
        #maps vowel to vowel appearing ranking
        vowel_map = {'a':0,'e':1,'i':2,'o':3,'u':4,'A':0,'E':1,'I':2,'O':3,'U':4}
        
        #constants
        lower_alpha = string.ascii_lowercase
        upper_alpha = string.ascii_uppercase
        lower_vowels = 'aeiou'
        upper_vowels = 'AEIOU'
        
        #dictionaries
        lower_dict = {}
        upper_dict = {}
        lower_shift_dict = {}
        reverse_lower_shift_dict = {}
        upper_shift_dict = {}
        reverse_upper_shift_dict = {}
        
        #assigns a vowel in the permutation a letter based on appearance
        for count, letter in enumerate(vowels_permutation.lower()):
            lower_shift_dict[letter] = count
            
        #same as above but for upper case vowels
        for count, letter in enumerate(vowels_permutation.upper()):
            upper_shift_dict[letter] = count


        for letter in lower_alpha:
            #if letter is a vowel, map it to the ranking in appearance
            #a will map to 0, e will map to 1, etc
            if letter in lower_vowels:
                lower_dict[letter] = vowel_map[letter]
                continue
            
            #if not vowel, map to itself
            lower_dict[letter] = letter
            
        #same as above but for upper case 
        for letter in upper_alpha:
            if letter in upper_vowels:
                upper_dict[letter] = vowel_map[letter]
                continue
            upper_dict[letter] = letter
        
        #reverses the dictionary but doesn't mutate the original dictionary
        for k,v in lower_shift_dict.items():
            reverse_lower_shift_dict[v] = k
            
        #same as above but for upper case 
        for k,v in upper_shift_dict.items():
            reverse_upper_shift_dict[v] = k
        
        for key in lower_dict.keys():
            if key not in lower_vowels:
                continue
            for vowel in lower_shift_dict:
                #if values are equal in appearance, map vowel to new vowel
                if lower_dict[key] == lower_shift_dict[vowel]:
                    lower_dict[key] = reverse_lower_shift_dict[lower_dict[key]]
        
        #same but for upper case
        for key in upper_dict.keys():
            if key not in upper_vowels:
                continue
            for vowel in upper_shift_dict:
                if upper_dict[key] == upper_shift_dict[vowel]:
                    upper_dict[key] = reverse_upper_shift_dict[upper_dict[key]]    
        
        #concatenates dictionary
        shifted_dict = dict(**lower_dict,**upper_dict)
        
        return shifted_dict
    
    def apply_transpose(self, transpose_dict):
        '''
        transpose_dict (dict): a transpose dictionary
        
        Returns: an encrypted version of the message text, based 
        on the dictionary
        '''
        #constants
        special_keys = ' !@#$%^&*()-_+={}[]|\:;\'<>?,./\"'
        
        self.transpose_dict = self.build_transpose_dict(self.vowels_permutation)
        transposed_message = ''
        
        #if letter in text, return the transposed version
        for letter in self.text:
            #if letter is a special key, return that special key
            if letter in special_keys:
                transposed_message += letter
                continue
            transposed_message += transpose_dict[letter]
        
        return transposed_message
        
class EncryptedSubMessage(SubMessage):
    def __init__(self, text):
        '''
        Initializes an EncryptedSubMessage object

        text (string): the encrypted message text

        An EncryptedSubMessage object inherits from SubMessage and has two attributes:
            self.message_text (string, determined by input text)
            self.valid_words (list, determined using helper function load_words)
        '''
        SubMessage.__init__(self, text)

    def decrypt_message(self):
        '''
        Attempt to decrypt the encrypted message 
        
        Idea is to go through each permutation of the vowels and test it
        on the encrypted message. For each permutation, check how many
        words in the decrypted text are valid English words, and return
        the decrypted message with the most English words.
        
        If no good permutations are found (i.e. no permutations result in 
        at least 1 valid word), return the original string. If there are
        multiple permutations that yield the maximum number of words, return any
        one of them.

        Returns: the best decrypted message    
        
        Hint: use your function from Part 4A
        '''
        list_of_words = load_words(WORDLIST_FILENAME)
        vowels = 'aeiou'
        possible_permutations = get_permutations(vowels)
        
        #tests all possible permutations
        for permutation in possible_permutations:
            transposed_dict = self.build_transpose_dict(permutation)
            decrypted_word = self.apply_transpose(transposed_dict)
            
            #if word is valid, return decrypted word
            if is_word(list_of_words, decrypted_word):
                return decrypted_word
            
            #if word consists of one or more words, check each word for validity
            #makes a copy
            decrypted_word_mod = decrypted_word
            
            #lower case and then splits word on space
            decrypted_word_mod = decrypted_word_mod.lower().split()
            
            #finds amount of words
            valid_length = len(decrypted_word_mod)
            
            #counter to check for how many valid words
            valid_word_count = 0
            
            #goes through each word
            for word in decrypted_word_mod:
                
                #if word is valid, add counter
                if word in list_of_words:
                    valid_word_count += 1
            
            #if counter is equal to amount of actual valid words, return decrypted message
            if valid_word_count == valid_length:
                return decrypted_word
        
        #returns original message if not decryptable
        return self.text
    

if __name__ == '__main__':

    # Example test case
    message = SubMessage("Hello World")
    permutation = "eaiuo"
    enc_dict = message.build_transpose_dict(permutation)
    print("Original message:", message.get_message_text(), "Permutation:", permutation)
    print("Expected encryption:", "Hallu Wurld")
    print("Actual encryption:", message.apply_transpose(enc_dict))
    enc_message = EncryptedSubMessage(message.apply_transpose(enc_dict))
    print("Decrypted message:", enc_message.decrypt_message())
     
    #TODO: WRITE YOUR TEST CASES HERE
