## Summary of Chapter 1

- The shell: the "Swiss Army Knife" of programmers
- Take a peek at the data; Perform simple data manipulations; Automation; Glue
- Hundreds of small programs, each carrying out a specific task
- "Worse is better"
- Connect input and output using "pipe" `|` and redirection `>` (or `>>`)

### Navigating the directory system

- To find out where you are: `pwd` (print working directory)
- Move one up with `cd ..`; move down using `cd wheretogo`
- Relative vs. absolute path: `cd /home/sallesina/CSB/unix` vs. `cd ../CSB/unix`
- Home directory `~`
- Go back to where you were last `cd -`
- List files and directories `ls`
- Command modifiers `ls -l` (long format)

### Working with files and directories

- Copy `cp myfile newloc`
- Move (or rename) `mv myfile newloc`
- Update file `touch myfile` (if file does not exist, it will be created)
- Remove file `rm myfile`
- Remove (empty) directory `rmdir mydir`
- Remove (non-empty) directory `rm -r mydir` **BEWARE!**
- First few lines `head myfile`
- Last few lines `tail -n3 myfile` (three lines)
- All but a few `tail -n+2 myfile` (all but first)
- Concactenate file(s) (or print to screen) `cat myfile1 myfile2`
- Count lines/words/bytes `wc myfile` (only lines `wc -l myfile`)
- Sort alphabetically lines `sort myfile`
- Sort numerically `sort -n myfile`
- Unique lines `uniq myfile` (with number of occurrences `uniq -c myfile`) **The file needs to be sorted!**

### Pipes and redirection

- Send text to a file `echo "my first line" > myfile.txt`
- Append text to a file `echo "my second line" >> myfile.txt`
- Pipe operator: take whatever is on the left, and use as input for the command on the right.
- `ls -l | wc -l` (count files and sub-directories in current directory)
- `sort myfile | uniq > no_duplicates.txt` sort and remove duplicates
- Can become complex: `history | awk '{print $2}' | sort | uniq -c | sort -rn | head -10` prints 10 most used commands

### Delimited text files

- Very common format to store data
- Delimiter can be comma, `Tab`, semicolon, special character
- You can select certain columns using `cut`
- `cut -d; -f3-5 mycsv.csv`: extract columns 3, 4, and 5 from `mycsv.csv`, which is a semicolon-separated file
- `cut -f3,6,9 myspace.tsv`: by default, separated by spaces/tabs; select non-adjacent columns

### Help with commands

- Very long manual: `man cut` **To exit, press `q`**

### Substituting characters

- `echo "aaabbbccc" | tr "a" "b"` changes `a` into `b`
- `echo "aaabbbccc" | tr -d "a"` removes all occurrences of `a`
- `echo "aaabbbccc" | tr -s "a"` squeezes all occurrences of `a`

### Warmup: Nobel data

The file `data/nobel.csv` contains the names and motivations for all the Nobel prizes awarded so far.

- Find the number of winners for each Nobel prize (`chemistry`, `economics`, `literature`, `medicine`, `peace`, `physics`). 

<details>
 <summary>Solution</summary>

tail -n+2 nobel.csv | cut -f3 -d, | sort | uniq -c | sort -nr

</details>

- Find the winners of multiple Nobel prizes

<details>
 <summary>Solution</summary>
   <p>

cut nobel.csv -f5-6 -d, | sort | uniq -c | sort -n -r | head -n10

  </p>
</details>


- Find the most common `surname`s among the winners

<details>
 <summary>Solution</summary>

cut nobel.csv -f6 -d, | sort | uniq -c | sort -nr | head -n10

</details>

- The Nobel prizes have not been awarder every year since 1901. Which one has been awarded the most? Which the least?

<details>
 <summary>Solution</summary>

tail -n+2 nobel.csv | cut -d, -f 2-3 | sort | uniq | cut -d, -f2 | sort | uniq -c | sort -nr

</details>

### Matching lines with grep

`grep` is based on regular expressions, which we will explore in Chapter 5. However, it is quite useful to work with literal matches (i.e., does the string appear in a line?). We will revisit this once we're familiar with regex, to see how powerful it can be.

- `grep "a string" a_file.txt` return the lines in `a_file.txt` that contain `a string`
- `grep -c "a string" a_file.txt` just count how many lines match
- `grep -w myword a_file.txt` just match entire words (i.e., flanked by spaces, punctuation, etc on the left and right)
- `grep -i MyWoRd a_file.txt` ignore case
- `grep -n myword a_file.txt` also print line numbers
- `grep -v myword a_file.txt` invert match: only return lines that do not contain the pattern
- `grep -o myword a_file.txt` only print the matched string (not the entire line)
- `grep -A 1 myword a_file.txt` also print the following line (`A` stands for `A`fter)
- `grep -B 3 myword a_file.txt` also print the preceding three lines (`B` stands for `B`efore)
- `grep "pattern_1\|pattern_2\|pattern_3" a_file.txt` match one of several patterns

### Warmup: Endangered species

Data taken from the [European Red List](https://www.eea.europa.eu/data-and-maps/data/european-red-lists-7)

Species codes:

1. **EX** Extinct
1. **RE** Regionally Extinct
1. **CR** Critically Endangered (= threatened species) 
1. **EN** Endangered (= threatened species)
1. **VU** Vulnerable (= threatened species)
1. **NT** Near Threatened
1. **LC** Least Concern
1. **DD** Data Deficient
1. **NA** Not Applicable

- Look at the first few lines to familiarize yourself with the data

<details>
 <summary>Solution</summary>

head European_Red_List.csv

</details>

- Count the number of occurrences for each category (`EX`, `RE`, etc.)

<details>
 <summary>Solution</summary>

tail -n+2 European_Red_List.csv | cut -d, -f10 | sort | uniq -c

</details>

- Repeat, but only consider birds (class `AVES`)

<details>
 <summary>Solution</summary>

grep -w AVES European_Red_List.csv | cut -d, -f10 | sort | uniq -c | sort -nr

</details>

- For each order of birds, compute the number of extinct/near extinct (`EX`, `RE` or `CE`) species

<details>
 <summary>Solution</summary>

grep AVES European_Red_List.csv | grep -w "CR\|EX\|RE" | cut -d, -f5,10 | sort | uniq -c

</details>

### Simple scripts

You can collect a series of commands into a **script**. Moreover, you can introduce generic arguments, making your script more general. For example, what does this program do?

```bash
#!/bin/bash
# the previousnot a comment: where to find the bash program

cat $1 | sort | uniq > nodup_$1
```

- Save the script as `sandbox/script1.sh`
- Make executable `chmod +rx script1.sh`
- Try running `./script1.sh test_file.txt`

A more complex example:

```bash
#!/bin/bash
# launch R code several times, 
# each time using a different seed for the random number generator

for i in `seq 1 $2`;
  do
    echo "Launching program $1 with seed $i"
    Rscript $1 $i
  done

```

- Save the script as `sandbox/script2.sh`
- Make executable `chmod +rx script2.sh`
- Launch `./script2.sh a_test.R 7`

### More resources on bash

- A [good collection](https://github.com/ruanyf/simple-bash-scripts) of very simple scripts, showing you the type of things you can do.
- Many good resources [here](https://github.com/awesome-lists/awesome-bash)
