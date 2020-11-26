# Problem Set 4B
# Name: <your name here>
# Collaborators:
# Time Spent: x:xx

import string

### HELPER CODE ###
def load_words(file_name):
    '''
    file_name (string): the name of the file containing 
    the list of words to load    
    
    Returns: a list of valid words. Words are strings of lowercase letters.
    
    Depending on the size of the word list, this function may
    take a while to finish.
    '''
    print("Loading word list from file...")
    # inFile: file
    inFile = open(file_name, 'r')
    # wordlist: list of strings
    wordlist = []
    for line in inFile:
        wordlist.extend([word.lower() for word in line.split(' ')])
    print("  ", len(wordlist), "words loaded.")
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

def get_story_string():
    """
    Returns: a story in encrypted text.
    """
    f = open("story.txt", "r")
    story = str(f.read())
    f.close()
    return story

### END HELPER CODE ###

WORDLIST_FILENAME = 'words.txt'

class Message(object):
    def __init__(self, text):
        '''
        Initializes a Message object
                
        text (string): the message's text

        a Message object has two attributes:
            self.message_text (string, determined by input text)
            self.valid_words (list, determined using helper function load_words)
        '''
        self.message_text = text
        self.valid_words = load_words(WORDLIST_FILENAME)
        

    def get_message_text(self):
        '''
        Used to safely access self.message_text outside of the class
        
        Returns: self.message_text
        '''
        return self.message_text

    def get_valid_words(self):
        '''
        Used to safely access a copy of self.valid_words outside of the class.
        This helps you avoid accidentally mutating class attributes.
        
        Returns: a COPY of self.valid_words
        '''
        return self.valid_words

    def build_shift_dict(self, shift):
        '''
        Creates a dictionary that can be used to apply a cipher to a letter.
        The dictionary maps every uppercase and lowercase letter to a
        character shifted down the alphabet by the input shift. The dictionary
        should have 52 keys of all the uppercase letters and all the lowercase
        letters only.        
        
        shift (integer): the amount by which to shift every letter of the 
        alphabet. 0 <= shift < 26

        Returns: a dictionary mapping a letter (string) to 
                 another letter (string). 
        '''
        
        #constants of different types of keys: 
        #separated based on case type and if special
        lower_alpha = string.ascii_lowercase
        upper_alpha = string.ascii_uppercase
        
        #dictionaries to store ranking to letter in alphabet
        #rank in alphabet is the value and letter is key
        lower_dict = {}
        upper_dict = {}
        
        #dictionaries to store shifted alphabet,
        #so 0 will be mapped to different letter
        lower_dict_shifted = lower_dict.copy()
        upper_dict_shifted = upper_dict.copy()
        
        #dictionaires to store reversed dictionaries
        #letter is now key and shifted ranking is now value
        reverse_lower_dict = {}
        reverse_upper_dict = {}
        reverse_lower_dict_shifted = {}
        reverse_upper_dict_shifted = {}
        
        #final dictionaires that map letter to shifted letter
        shifted_dict_lower = {}
        shifted_dict_upper = {}
        
        #assigns a ranking to each letter in alphabet from 0-25
        #only for lower case letter
        for count,letter in enumerate(lower_alpha):
            #shifts ranking to right based on user-input
            shifted = count + shift
        
        #ensures that ranking is between 0 and 25
            if shifted > 25:
                shifted -= 26

        #adds normal ranking of alphabet to dictionary
        #values are the ranking and keys are the letters
            lower_dict[count] = letter
        
        #adds shifted ranking of alphabet to dictionary
        #values are the shifted ranking and keys are the letters
            lower_dict_shifted[shifted] = letter
            
        #does the same as code above but for upper case
        for count,letter in enumerate(upper_alpha):
            shifted = count + shift
            if shifted > 25:
                shifted -= 26
            upper_dict[count] = letter
            upper_dict_shifted[shifted] = letter

        #reverses the order of the ranked alphabet dictionary
        #ranking is now the values and letter is the keys
        for k,v in lower_dict_shifted.items():
            reverse_lower_dict_shifted[v] = k
        
        for k,v in lower_dict.items():
            reverse_lower_dict[v] = k
            
        #does the same as code above but for upper case letters
        for k,v in upper_dict_shifted.items():
            reverse_upper_dict_shifted[v] = k
            
        for k,v in upper_dict.items():
            reverse_upper_dict[v] = k
        
        #iterates through normal dictionary
        for element in reverse_lower_dict:
            #iterates through shifted dictionary
            for key in reverse_lower_dict_shifted:
                #if the values are the same, meaning map letter original to shifted letter
                if (reverse_lower_dict[element]+4) == reverse_lower_dict_shifted[key]:
                    shifted_dict_lower[element] = key
                    
        #same as above but for upper case letters
        for element in reverse_upper_dict:
            for key in reverse_upper_dict_shifted:
                if (reverse_upper_dict[element]+4) == reverse_upper_dict_shifted[key]:
                    shifted_dict_upper[element] = key
                
        #concatenates dictionary
        shifted_dict = dict(**shifted_dict_lower, **shifted_dict_upper)
        
        return shifted_dict
        

    def apply_shift(self, shift):
        '''
        Applies the Caesar Cipher to self.message_text with the input shift.
        Creates a new string that is self.message_text shifted down the
        alphabet by some number of characters determined by the input shift        
        
        shift (integer): the shift with which to encrypt the message.
        0 <= shift < 26

        Returns: the message text (string) in which every character is shifted
             down the alphabet by the input shift
        '''
        #constants
        special_keys = ' !@#$%^&*()-_+={}[]|\:;\'<>?,./\"'
        
        #message after the shift
        ciphered_message = ''
        
        #builds shifted dictionary
        shifted_dict = self.build_shift_dict(shift)
        
        #reiterates through every letter in text and returns shifted version
        for letter in self.message_text:
            #if letter is a special key, returns the special key
            if letter in special_keys:
                ciphered_message += letter
            
            else:
                ciphered_message += shifted_dict[letter]
 
        return ciphered_message

class PlaintextMessage(Message):
    def __init__(self, text, shift):
        '''
        Initializes a PlaintextMessage object        
        
        text (string): the message's text
        shift (integer): the shift associated with this message

        A PlaintextMessage object inherits from Message and has five attributes:
            self.message_text (string, determined by input text)
            self.valid_words (list, determined using helper function load_words)
            self.shift (integer, determined by input shift)
            self.encryption_dict (dictionary, built using shift)
            self.message_text_encrypted (string, created using shift)

        '''
        Message.__init__(self,text)
        self.shift = shift
        self.encryption_dict = self.build_shift_dict(shift)
        self.message_text_encrypted = self.apply_shift(shift)

    def get_shift(self):
        '''
        Used to safely access self.shift outside of the class
        
        Returns: self.shift
        '''
        return self.shift

    def get_encryption_dict(self):
        '''
        Used to safely access a copy self.encryption_dict outside of the class
        
        Returns: a COPY of self.encryption_dict
        '''
        return self.encryption_dict.copy()

    def get_message_text_encrypted(self):
        '''
        Used to safely access self.message_text_encrypted outside of the class
        
        Returns: self.message_text_encrypted
        '''
        return self.message_text_encrypted

    def change_shift(self, shift):
        '''
        Changes self.shift of the PlaintextMessage and updates other 
        attributes determined by shift.        
        
        shift (integer): the new shift that should be associated with this message.
        0 <= shift < 26

        Returns: nothing
        '''
        self.shift = shift
        self.build_shift_dict(shift)
        
        return None


class CiphertextMessage(Message):
    def __init__(self, text):
        '''
        Initializes a CiphertextMessage object
                
        text (string): the message's text

        a CiphertextMessage object has two attributes:
            self.message_text (string, determined by input text)
            self.valid_words (list, determined using helper function load_words)
        '''
        Message.__init__(self,text)

    def decrypt_message(self):
        '''
        Decrypt self.message_text by trying every possible shift value
        and find the "best" one. We will define "best" as the shift that
        creates the maximum number of real words when we use apply_shift(shift)
        on the message text. If s is the original shift value used to encrypt
        the message, then we would expect 26 - s to be the best shift value 
        for decrypting it.

        Note: if multiple shifts are equally good such that they all create 
        the maximum number of valid words, you may choose any of those shifts 
        (and their corresponding decrypted messages) to return

        Returns: a tuple of the best shift value used to decrypt the message
        and the decrypted message text using that shift value
        '''
        list_of_words = load_words(WORDLIST_FILENAME)
        
        #checks every possible shift 
        for i in range(25):
            ciphered_message = self.apply_shift(i)
            
            #if the ciphered message is a valid word, return shift,word
            if is_word(list_of_words,ciphered_message):
                return (i,ciphered_message)
                
            

if __name__ == '__main__':

#    #Example test case (PlaintextMessage)
#    plaintext = PlaintextMessage('hello', 2)
#    print('Expected Output: jgnnq')
#    print('Actual Output:', plaintext.get_message_text_encrypted())
#
#    #Example test case (CiphertextMessage)
#    ciphertext = CiphertextMessage('jgnnq')
#    print('Expected Output:', (24, 'hello'))
#    print('Actual Output:', ciphertext.decrypt_message())

    #TODO: WRITE YOUR TEST CASES HERE

    #TODO: best shift value and unencrypted story 
    
     plaintext = PlaintextMessage('hello', 2)
     print('Expected Output: jgnnq')
     print('Actual Output:', plaintext.get_message_text_encrypted())

     ciphertext = CiphertextMessage('jgnnq')
     print('Expected Output:', (24, 'hello'))
     print('Actual Output:', ciphertext.decrypt_message())
     
     cipherstory = CiphertextMessage(get_story_string())
     print(cipherstory.decrypt_message())
