import re

# 1. For each author, use regular expressions to extact:
# Name
# Number of papers
# The first in the list of disciplines

# read all of the html in one go
with open('scopus.html', 'r') as content_file:
    content = content_file.read()

# compile the regular expression for faster execution
# we are simply capturing with a group whatever follows "View this author&#39;s profile">
find_author = re.compile(r'title="View this author&#39;s profile"> (.*)<')
author_list = re.findall(find_author, content)

# Similarly, capture the number of papers
find_num_papers = re.compile(r'"View documents for this author" class="txtSmaller">\n(.*)')
paper_list = re.findall(find_num_papers, content)

# And finally, the discipline
find_disciplines = re.compile(r'<td class="dataCol4">\n(.*)')
discipline_list = re.findall(find_disciplines, content)

# compile a better-organized list of authors
all_authors = []
for i in range(len(author_list)):
   all_authors.append([author_list[i].strip(),
                       int(paper_list[i]),
                       discipline_list[i].strip()])

print("Exercise 1")

for i in range(10):
   print(all_authors[i])

print("")

# 2. Which discipline is most represented? 
# Take the list of authors you’ve just produced, 
# and tally the number of authors by discipline.

# organize in a dictionary
disciplines = {}
for au in all_authors:
   my_disc = au[2]
   if my_disc not in disciplines.keys():
      disciplines[my_disc] = 0
   disciplines[my_disc] = disciplines[my_disc] + 1 

print("Exercise 2")

for key,value in disciplines.items():
    print(key, value)

print("")

# 3. What is the average number of papers for these authors?

mean_papers = 0
# add all papers together and divide by 2000
for au in all_authors:
    mean_papers = mean_papers + au[1]
mean_papers = mean_papers / len(all_authors)

print("Exercise 3")

print("Average:", round(mean_papers, 2))

print("")

