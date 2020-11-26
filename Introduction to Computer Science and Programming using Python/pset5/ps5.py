# 6.0001/6.00 Problem Set 5 - RSS Feed Filter
# Name:
# Collaborators:
# Time:

import feedparser
import string
import time
import threading
from project_util import translate_html
from mtTkinter import *
from datetime import datetime
import pytz

#-----------------------------------------------------------------------

#======================
# Code for retrieving and parsing
# Google and Yahoo News feeds
# Do not change this code
#======================

def process(url):
    """
    Fetches news items from the rss url and parses them.
    Returns a list of NewsStory-s.
    """
    feed = feedparser.parse(url)
    entries = feed.entries
    ret = []
    for entry in entries:
        guid = entry.guid
        title = translate_html(entry.title)
        link = entry.link
        description = translate_html(entry.description)
        pubdate = translate_html(entry.published)

        try:
            pubdate = datetime.strptime(pubdate, "%a, %d %b %Y %H:%M:%S %Z")
            pubdate.replace(tzinfo=pytz.timezone("GMT"))
          #  pubdate = pubdate.astimezone(pytz.timezone('EST'))
          #  pubdate.replace(tzinfo=None)
        except ValueError:
            pubdate = datetime.strptime(pubdate, "%a, %d %b %Y %H:%M:%S %z")

        newsStory = NewsStory(guid, title, description, link, pubdate)
        ret.append(newsStory)
    return ret

#======================
# Data structure design
#======================

# Problem 1

class NewsStory(object):
    def __init__(self, guid, title, description, link, pubdate):
        '''
        Takes in parameters globall unique identifier (GUID), title, 
        description, link, and publication date.
        '''
        self.guid = guid
        self.title = title
        self.description = description
        self.link = link
        self.pubdate = pubdate
    
    def get_guid(self):
        #gets GUID
        return self.guid
    
    def get_title(self):
        #gets title
        return self.title
    
    def get_description(self):
        #gets decription
        return self.description
    
    def get_link(self):
        #gets link
        return self.link
    
    def get_pubdate(self):
        #gets publication date
        return self.pubdate
        

#======================
# Triggers
#======================

class Trigger(object):
    def evaluate(self, story):
        """
        Returns True if an alert should be generated
        for the given news item, or False otherwise.
        """
        # DO NOT CHANGE THIS!
        raise NotImplementedError


# PHRASE TRIGGERS

# Problem 2

class PhraseTrigger(Trigger):
    def __init__(self, phrase):
        self.phrase = phrase
        
    def is_phrase_in(self, text):
        '''
        checks to see if phrase is in the string 
        returns True if it is and False if not
        '''
        #string containing all punctuation
        punc = string.punctuation
        
        #empty dicionaries
        phrase_dict = {}
        text_dict = {}

        #turns phrase and text lowercase 
        phrase = self.phrase.lower()
        text = text.lower()
        
        #removes all punctuation and replaces with space
        for element in punc:
            text = text.split(element)
            text = ' '.join(text)
        
        #splits phrase and text by spaces into lists
        phrase = phrase.split()
        text = text.split()    
        
        #gives appearance ranking to each word in phrase
        for count, element in enumerate(phrase):
            phrase_dict[count] = element
            
        #gives appearance ranking to each word in text
        for count, element in enumerate(text):
            text_dict[count] = element
        
        #makes sure if in order
        try:
            #gives index of where first word of phrase appears in text
            first_word_index = text.index(phrase_dict[0])
            
            #checks to make sure if words appear consecutively
            for i in range(len(phrase_dict)):
                if phrase_dict[i] != text_dict[first_word_index + i]:
                    return False
                
        #if words not in order, return False
        except:
            return False
        
        #if one word in phrase is not in text, return False
        for element in phrase:
            if element not in text:
                return False
                
        return True
            

# Problem 3
class TitleTrigger(PhraseTrigger):
    '''
    Triggers if phrase is in title of NewsStory
    '''
    def evaluate(self, story):
        return self.is_phrase_in(story.get_title())
    

# Problem 4
class DescriptionTrigger(PhraseTrigger):
    '''
    Triggers if phrase is in description of NewsStory
    '''
    def evaluate(self, story):
        return self.is_phrase_in(story.get_description())


# TIME TRIGGERS

# Problem 5
class TimeTrigger(Trigger):
    '''
    Takes in time as string 'day month_abbreviated year hours:minutes:seconds'
    Returns in DateTime format
    '''
    def __init__(self,time):
        time = datetime.strptime(time,"%d %b %Y %H:%M:%S")
        time = time.replace(tzinfo=pytz.timezone("EST"))
        self.time = time
        

# Problem 6
class BeforeTrigger(TimeTrigger):
    '''
    Returns True if publication date is before time
    '''
    def evaluate(self, story):
        pubdate = story.get_pubdate().replace(tzinfo=pytz.timezone("EST"))
        return pubdate < self.time
        
        
class AfterTrigger(TimeTrigger):
    '''
    Returns True if publication date is after time
    '''
    def evaluate(self, story):
        pubdate = story.get_pubdate().replace(tzinfo=pytz.timezone("EST"))
        return pubdate > self.time


# COMPOSITE TRIGGERS

# Problem 7
class NotTrigger(Trigger):
    '''
    Takes a trigger as argument and returns the opposite Boolean value
    '''
    def __init__(self, trigger):
        self.trigger = trigger
        
    def evaluate(self, story):
        return not self.trigger.evaluate(story)
    

# Problem 8
class AndTrigger(Trigger):
    '''
    Takes two triggers and returns True if both triggers are True
    '''
    def __init__(self, trigger1, trigger2):
        self.trigger1 = trigger1
        self.trigger2 = trigger2
        
    def evaluate(self, story):
        return (self.trigger1.evaluate(story) and self.trigger2.evaluate(story))


# Problem 9
class OrTrigger(Trigger):
    '''
    Takes two triggers and returns True if either triggers are True
    '''
    def __init__(self, trigger1, trigger2):
        self.trigger1 = trigger1
        self.trigger2 = trigger2
        
    def evaluate(self, story):
        return (self.trigger1.evaluate(story) or self.trigger2.evaluate(story))
    

#======================
# Filtering
#======================

# Problem 10
def filter_stories(stories, triggerlist):
    """
    Takes in a list of NewsStory instances.

    Returns: a list of only the stories for which a trigger in triggerlist fires.
    """
    #makes copy of story to avoid mutation of list while removing
    stories_copy = stories.copy()
    
    #checks each story
    for story in stories_copy:
        trigger_count = 0
        
        #checks each trigger on the story
        for trigger in triggerlist:
            #applies each trigger on the story
            #if trigger returns True, increase count by one
            if trigger.evaluate(story):
                trigger_count += 1
                
        #if no triggers activate, remove story from list
        if trigger_count == 0:
            stories.remove(story)
    
    return stories


#======================
# User-Specified Triggers
#======================
# Problem 11
def read_trigger_config(filename):
    """
    filename: the name of a trigger configuration file

    Returns: a list of trigger objects specified by the trigger configuration
        file.
    """
    # We give you the code to read in the file and eliminate blank lines and
    # comments. You don't need to know how it works for now!
    trigger_file = open(filename, 'r')
    lines = []
    for line in trigger_file:
        line = line.rstrip()
        if not (len(line) == 0 or line.startswith('//')):
            lines.append(line)

    # TODO: Problem 11
    trig_dict = {}
    trig_list = []
    for element in lines:
        element = element.split(',')
        if element[1] == 'TITLE':
            trig_dict[element[0]] = TitleTrigger(element[2])
        elif element[1] == 'DESCRIPTION':
            trig_dict[element[0]] = DescriptionTrigger(element[2])
        elif element[1] == 'AFTER':
            trig_dict[element[0]] = AfterTrigger(element[2])
        elif element[1] == 'BEFORE':
           trig_dict[element[0]] = BeforeTrigger(element[2])
        elif element[1] == 'NOT':
            trig_dict[element[0]] = NotTrigger(element[2])
        elif element[1] == 'AND':
            trig_dict[element[0]] = AndTrigger(trig_dict[element[2]], trig_dict[element[3]])
        elif element[0] == 'ADD':
            for trigger in element:
                if trigger != 'ADD':
                    trig_list.append(trig_dict[trigger])
    return trig_list



SLEEPTIME = 120 #seconds -- how often we poll

def main_thread(master):
    # A sample trigger list - you might need to change the phrases to correspond
    # to what is currently in the news
    try:
        t1 = TitleTrigger("Biden")
        t2 = TitleTrigger("Trump")
        t3 = DescriptionTrigger("Biden")
        t6 = AndTrigger(t1, t2)
        triggerlist = [t6]

        # Problem 11
        #TODO: After implementing read_trigger_config, uncomment this line 
        triggerlist = read_trigger_config('triggers.txt')
        
        # HELPER CODE - you don't need to understand this!
        # Draws the popup window that displays the filtered stories
        # Retrieves and filters the stories from the RSS feeds
        frame = Frame(master)
        frame.pack(side=BOTTOM)
        scrollbar = Scrollbar(master)
        scrollbar.pack(side=RIGHT,fill=Y)

        t = "Google Top News"
        title = StringVar()
        title.set(t)
        ttl = Label(master, textvariable=title, font=("Helvetica", 18))
        ttl.pack(side=TOP)
        cont = Text(master, font=("Helvetica",14), yscrollcommand=scrollbar.set)
        cont.pack(side=BOTTOM)
        cont.tag_config("title", justify='center')
        button = Button(frame, text="Exit", command=root.destroy)
        button.pack(side=BOTTOM)
        guidShown = []
        def get_cont(newstory):
            if newstory.get_guid() not in guidShown:
                cont.insert(END, newstory.get_title()+"\n", "title")
                cont.insert(END, "\n---------------------------------------------------------------\n", "title")
                cont.insert(END, newstory.get_description())
                cont.insert(END, "\n*********************************************************************\n", "title")
                guidShown.append(newstory.get_guid())

        while True:

            print("Polling . . .", end=' ')
            # Get stories from Google's Top Stories RSS news feed
            stories = process("http://news.google.com/news?output=rss")

            stories = filter_stories(stories, triggerlist)

            list(map(get_cont, stories))
            scrollbar.config(command=cont.yview)


            print("Sleeping...")
            time.sleep(SLEEPTIME)

    except Exception as e:
        print(e)


if __name__ == '__main__':
    root = Tk()
    root.title("Some RSS parser")
    t = threading.Thread(target=main_thread, args=(root,))
    t.start()
    root.mainloop()

