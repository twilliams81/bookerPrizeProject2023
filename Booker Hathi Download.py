#!/usr/bin/env python
# coding: utf-8

# In[ ]:


get_ipython().system('pip install htrc-feature-reader')
get_ipython().system('pip install pyLDAvis')
get_ipython().system('pip install htrc')

import pandas as pd
from htrc_features import FeatureReader, Volume
from tqdm.notebook import trange, tqdm
import os
import shutil
import nltk
#import gensim

# Because some of our libraries will throw many, many warnings for future changes to their code:
import warnings
warnings.filterwarnings("ignore", category=DeprecationWarning)

os.chdir('/Users/tomwilliams/Library/Mobile Documents/com~apple~CloudDocs/UVA/Thesis/Code')

from htrc import workset
bookerVolIds = workset.load_hathitrust_collection('https://babel.hathitrust.org/cgi/mb?a=listis&c=1335516225')
FeatureReader(ids=bookerVolIds).first().title#This checks first title in the list
#This creates a list of volume IDs for me to work with

x = bookerVolIds[0:10]
print(x)

def ef_vol_to_bow_df(volume, save_to_tsv=False):
    from htrc_features import FeatureReader, Volume
    import pandas as pd
    from tqdm.notebook import trange, tqdm
    import os
    notebook_path = os.getcwd()
    htid = volume.id
    # creating an empty Dataframe to which we'll add clean tokens, instead of a text file
    vol_df = pd.DataFrame(columns=['htid', 'page_number', 'page_tokens'])
    outfile_name = htid+'.tsv' # saving our volume-level token DataFrame to a TSV
    for page in tqdm(volume.pages(), total=volume.page_count):
        page_num = str(page).split(' ')[1]
        page_df = page.tokenlist(section='body', case=False, pos=False)
        tkn_list = []
        
        for i, r in page_df.iterrows():
            tkn = i[2]
            clean_tkn = tkn.strip()
            count = r[0]
            tkns = ([f'{clean_tkn}'] * count)
            clean_tkn_list = [word for word in tkns if word.isalpha()]
            clean_tkn_list = [word for word in clean_tkn_list if word not in en_stop]
            tkn_list.extend(clean_tkn_list)
        '''
        Instead of writing to text files, we are adding the page-level clean tokens to our
        DataFrame, with one each page of tokens constituting one row in the DataFrame.
        '''
        vol_df = vol_df.append({'htid': htid, 'page_number':  page_num, 'page_tokens': tkn_list}, ignore_index=True)
    '''
    Lastly, we save our volume-level DataFrame as a tab-separated file, and return the volume 
    DataFrame so that we can better aggregate each volume's tokens into a single DataFrame (you'll 
    see this code in the wrapper we write to iterate through multiple volumes)
    '''
    if save_to_tsv==True:
        vol_df.to_csv(outfile_name, sep='\t', index=False)
        print(f'Saved {volume.title} to TSV named {outfile_name}')
    
    print(f'Reformatted "{volume.title}" ({htid}) to bag-of-words')
    return vol_df

nltk.download('stopwords')
en_stop = set(nltk.corpus.stopwords.words('english'))

en_stop.add("'")
en_stop.add('"')
en_stop.add(' ')
en_stop.add('would')
en_stop.add('could')
en_stop.add('should')
en_stop.add('said')
en_stop.add('also')

# If we wanted to be a bit more clever, we could use a simple loop to add these words to our stop list with less typing:

# stop_words_to_add = ["'",'"',' ','would','could','should','also', 'said']

# for word in stop_words_to_add:
#     en_stop.add(word)

workset_page_df = pd.DataFrame(columns=['htid','page_number','page_tokens'])

for book in tqdm(bookerVolIds):
    fr_vol = Volume(book)
    book_df = ef_vol_to_bow_df(fr_vol, save_to_tsv=True)
    workset_page_df = workset_page_df.append(book_df)
    
print(f"Reformatted {len(volume_list)} volumes to bag-of-words pages.")

#I couldn't work out why the code was breaking and needed to fiddle with the text to work out which books caused the problems.

x = bookerVolIds[32]
print(x)

y = bookerVolIds[150]
print(y)


z = bookerVolIds[181]
print(z)

a = bookerVolIds[215]
print(a)

#bookerVolIds.remove(x)
#bookerVolIds.remove(y)
#bookerVolIds.remove(z)
#bookerVolIds.remove(a)

#These 4 volumes caused the next cell to crash. They are

print(x,y,z,a)

workset_page_df = pd.DataFrame(columns=['htid','page_number','page_tokens'])

for book in tqdm(bookerVolIds):
    fr_vol = Volume(book)
    book_df = ef_vol_to_bow_df(fr_vol, save_to_tsv=True)
    workset_page_df = workset_page_df.append(book_df)
    
print(f"Reformatted {len(volume_list)} volumes to bag-of-words pages.")

missingVolIds = ["ucbk.ark+=28722=h24m91c3s", "ucbk.ark+=228722=h27940w7z","ucbk.ark+=228722=h2tx35d4d","ucbk.ark+=228722=h2x34n702"]

print(missingVolIds)

workset_page_df2 = pd.DataFrame(columns=['htid','page_number','page_tokens'])

for book in tqdm(missingVolIds):
    fr_vol = Volume(book)
    book_df = ef_vol_to_bow_df(fr_vol, save_to_tsv=True)
    workset_page_df = workset_page_df2.append(book_df)
    
print(f"Reformatted {len(volume_list)} volumes to bag-of-words pages.")


