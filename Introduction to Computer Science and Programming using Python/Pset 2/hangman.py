# Problem Set 2, hangman.py
# Name: 
# Collaborators:
# Time spent:

# Hangman Game
# -----------------------------------
# Helper code
# You don't need to understand this helper code,
# but you will have to know how to use the functions
# (so be sure to read the docstrings!)
import random
import string

WORDLIST_FILENAME = "words.txt"


def load_words():
    """
    Returns a list of valid words. Words are strings of lowercase letters.
    
    Depending on the size of the word list, this function may
    take a while to finish.
    """
    print("Loading word list from file...")
    # inFile: file
    inFile = open(WORDLIST_FILENAME, 'r')
    # line: string
    line = inFile.readline()
    # wordlist: list of strings
    wordlist = line.split()
    print("  ", len(wordlist), "words loaded.")
    return wordlist



def choose_word(wordlist):
    """
    wordlist (list): list of words (strings)
    
    Returns a word from wordlist at random
    """
    return random.choice(wordlist)

# end of helper code

# -----------------------------------

# Load the list of words into the variable wordlist
# so that it can be accessed from anywhere in the program
wordlist = load_words()


def is_word_guessed(secret_word, letters_guessed):
    '''
    secret_word: string, the word the user is guessing; assumes all letters are
      lowercase
    letters_guessed: list (of letters), which letters have been guessed so far;
      assumes that all letters are lowercase
    returns: boolean, True if all the letters of secret_word are in letters_guessed;
      False otherwise
    '''
    guessed = None
    #iterates through all the letters in secret_word
    #if all the letters in secret_word are in letters_guessed, word is guessed
    for letter in secret_word:
        if letter in letters_guessed:
            guessed = True
        #if one letter in secret_word is not in list of letters_guessed, word is not guessed
        else:
            guessed = False
            break
    return guessed



def get_guessed_word(secret_word, letters_guessed):
    '''
    secret_word: string, the word the user is guessing
    letters_guessed: list (of letters), which letters have been guessed so far
    returns: string, comprised of letters, underscores (_), and spaces that represents
      which letters in secret_word have been guessed so far.
    '''
    guess = ''
    #iterates through secret_word and returns the word with the letters guessed so far
    for letter in secret_word:
        if letter in letters_guessed:
            guess += letter
        #if letter has not been guessed yet, return an underscore and space
        else:
            guess += '_ '
    return guess



def get_available_letters(letters_guessed):
    '''
    letters_guessed: list (of letters), which letters have been guessed so far
    returns: string (of letters), comprised of letters that represents which letters have not
      yet been guessed.
    '''
    all_lowercase_letters = string.ascii_lowercase
    letters_left = ''
    #creates list of letters that have not been guessed yet
    for letter in all_lowercase_letters:
        if letter not in letters_guessed:
            letters_left += letter
    return letters_left
    
    

def hangman(secret_word):
    '''
    secret_word: string, the secret word to guess.
    
    Starts up an interactive game of Hangman.
    
    * At the start of the game, let the user know how many 
      letters the secret_word contains and how many guesses s/he starts with.
      
    * The user should start with 6 guesses

    * Before each round, you should display to the user how many guesses
      s/he has left and the letters that the user has not yet guessed.
    
    * Ask the user to supply one guess per round. Remember to make
      sure that the user puts in a letter!
    
    * The user should receive feedback immediately after each guess 
      about whether their guess appears in the computer's word.

    * After each guess, you should display to the user the 
      partially guessed word so far.
    
    Follows the other limitations detailed in the problem write-up.
    '''
    total_score = 0
    print('Welcome to the game of Hangman!')
    count = 0
    #counts how many letters are in the secret word
    for letter in secret_word:
        count += 1
    word_length = count
    print("I am thinking of a word that is",count,"letters long")
    print("-------------")
    guess_count = 6
    print("You have",guess_count,"guesses left")
    warnings = 3
    print("You have",warnings,"warnings left")
    letters_guessed = ''
    print("Available letters:", get_available_letters(letters_guessed))
    #keeps on running if word has not been guessed yet
    while not is_word_guessed(secret_word, letters_guessed):
        #if person runs out of guesses, program stops
        if guess_count == 0:
            break
        user_guess = input("Please guess a letter: ")
        user_guess = user_guess.lower()
        #adds user guess into list of letters_guessed
        if user_guess not in letters_guessed and user_guess in string.ascii_lowercase:
            letters_guessed += user_guess
        #if the user has already guessed the same letter or inputs an invalid input, reduces amount of warnings
        elif user_guess in letters_guessed or user_guess not in string.ascii_lowercase:
            if warnings > 0:
                warnings -=1
                print("Oops! That is not a valid letter. You have",warnings,"warnings left.")
                print(get_guessed_word(secret_word,letters_guessed))
            #if user runs out of warnings, loses guessed instead
            elif warnings == 0:
                guess_count -= 1
                print("Oops! That is not a valid letters. You have lost a guess")
                print(get_guessed_word(secret_word,letters_guessed))
        #if user's letter is in secret word, congratulate them
        if user_guess in secret_word:
            print("Good guess:", get_guessed_word(secret_word,letters_guessed))
        #if user's letter is not in secret word, deduct guesses and give feedback
        elif user_guess not in secret_word:
            print("Oops! That letter is not in my word:", get_guessed_word(secret_word,letters_guessed))
            #if user's letter is an vowel, deduct 2 guesses
            if user_guess in "aeiou":
                guess_count -= 2
            #if user's letter is not a vowel, deduct only 1 guess
            else:
                guess_count -= 1
        print("-------------")
        print("You have",guess_count,"guesses left")
        print("Available letters:", get_available_letters(letters_guessed))
    #if word is guessed, give score depending on length of word and guesses remaining
    if is_word_guessed(secret_word, letters_guessed) == True:
        print("-------------")
        print("Congratulations! The word was", secret_word)
        total_score = guess_count * word_length
        print("Your total score for this game is:", total_score)
    #if word is not guessed, say game over
    else:
        print("-------------")
        print("Game Over! The word was", secret_word)


# When you've completed your hangman function, scroll down to the bottom
# of the file and uncomment the first two lines to test
#(hint: you might want to pick your own
# secret_word while you're doing your own testing)


# -----------------------------------

def match_with_gaps(my_word, other_word):
    '''
    my_word: string with _ characters, current guess of secret word
    other_word: string, regular English word
    returns: boolean, True if all the actual letters of my_word match the 
        corresponding letters of other_word, or the letter is the special symbol
        _ , and my_word and other_word are of the same length;
        False otherwise: 
    '''
    match = None
    #removes spaces from my_word
    my_word_split = my_word.split(' ')
    my_word_split = ''.join(my_word_split)
    list_of_matching = ''
    matching_count_other = 0
    matching_count_my = 0
    
    for i in range(len(other_word)):
        #if length of words don't match, words can't possibly match
        if len(my_word_split) != len(other_word):
            match = False
            break
        #checks every letter to see if they match 
        if (my_word_split[i] == other_word[i]) or (my_word_split[i] == '_'):
            match = True
        #if one letter doesn't match, then doesn't match
        else:
            match = False
            break
        #if letters match, create list of matching letters
        if other_word[i] == my_word_split[i]:
            list_of_matching += other_word[i]
    #checks how many letters match 
    for letter in other_word:
        if letter in list_of_matching:
            matching_count_other += 1
    for letter in my_word_split:
        if letter in list_of_matching:
            matching_count_my += 1
    #if there is a difference in letters matching, they don't match
    #used in order to prevent some matching of words
    #example, 't_ _ _ ' won't match 'tact' 
    if matching_count_other != matching_count_my:
        match = False
    return match



def show_possible_matches(my_word):
    '''
    my_word: string with _ characters, current guess of secret word
    returns: nothing, but should print out every word in wordlist that matches my_word
             Keep in mind that in hangman when a letter is guessed, all the positions
             at which that letter occurs in the secret word are revealed.
             Therefore, the hidden letter(_ ) cannot be one of the letters in the word
             that has already been revealed.

    '''
    list_of_words = ''
    #if the word could possibly match the secret word, create list of possible matches
    for word in wordlist:
        if match_with_gaps(my_word, word) == True:
            list_of_words += word + ' '
    return list_of_words


def hangman_with_hints(secret_word):
    '''
    secret_word: string, the secret word to guess.
    
    Starts up an interactive game of Hangman.
    
    * At the start of the game, let the user know how many 
      letters the secret_word contains and how many guesses s/he starts with.
      
    * The user should start with 6 guesses
    
    * Before each round, you should display to the user how many guesses
      s/he has left and the letters that the user has not yet guessed.
    
    * Ask the user to supply one guess per round. Make sure to check that the user guesses a letter
      
    * The user should receive feedback immediately after each guess 
      about whether their guess appears in the computer's word.

    * After each guess, you should display to the user the 
      partially guessed word so far.
      
    * If the guess is the symbol *, print out all words in wordlist that
      matches the current guessed word. 
    
    Follows the other limitations detailed in the problem write-up.
    '''
    total_score = 0
    print('Welcome to the game of Hangman!')
    count = 0
    for letter in secret_word:
        count += 1
    word_length = count
    print("I am thinking of a word that is",count,"letters long")
    print("-------------")
    guess_count = word_length * 2
    print("You have",guess_count,"guesses left")
    warnings = 3
    print("You have",warnings,"warnings left")
    letters_guessed = ''
    print("Available letters:", get_available_letters(letters_guessed))
    print("For list of possible words, type '*'")
    while not is_word_guessed(secret_word, letters_guessed):
        if guess_count == 0:
            break
        user_guess = input("Please guess a letter: ")
        #if user inputs an asterisk, gives list of possible words
        if user_guess == '*':
            print(show_possible_matches(get_guessed_word(secret_word, letters_guessed)))
        user_guess = user_guess.lower()
        if user_guess not in letters_guessed and user_guess in string.ascii_lowercase:
            letters_guessed += user_guess
        elif user_guess in letters_guessed or user_guess not in string.ascii_lowercase:
            if warnings > 0 and user_guess != '*':
                warnings -=1
                print("Oops! That is not a valid letter. You have",warnings,"warnings left.")
                print(get_guessed_word(secret_word,letters_guessed))
            elif warnings == 0:
                guess_count -= 1
                print("Oops! That is not a valid letters. You have lost a guess")
                print(get_guessed_word(secret_word,letters_guessed))
        if user_guess in secret_word:
            print("Good guess:", get_guessed_word(secret_word,letters_guessed))
        elif user_guess not in secret_word and user_guess != '*':
            print("Oops! That letter is not in my word:", get_guessed_word(secret_word,letters_guessed))
            if user_guess in "aeiou":
                guess_count -= 2
            else:
                guess_count -= 1
        print("-------------")
        print("You have",guess_count,"guesses left")
        print("Available letters:", get_available_letters(letters_guessed))
    if is_word_guessed(secret_word, letters_guessed) == True:
        print("-------------")
        print("Congratulations! The word was", secret_word)
        total_score = guess_count * word_length
        print("Your total score for this game is:", total_score)
    else:
        print("-------------")
        print("Game Over! The word was", secret_word)



# When you've completed your hangman_with_hint function, comment the two similar
# lines above that were used to run the hangman function, and then uncomment
# these two lines and run this file to test!
# Hint: You might want to pick your own secret_word while you're testing.


if __name__ == "__main__":
    pass

    # To test part 2, comment out the pass line above and
    # uncomment the following two lines.
    
    #secret_word = choose_word(wordlist)
    #hangman(secret_word)

###############
    
    # To test part 3 re-comment out the above lines and 
    # uncomment the following two lines. 
    
    secret_word = choose_word(wordlist)
    hangman_with_hints(secret_word)
