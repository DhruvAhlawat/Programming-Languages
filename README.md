## Some design decisions I took
(Note that there are some whitespace characters that are not spaces in the mdtab-2023.mdt file, so please backspace and put the spaces again, or shift+Tab the whole and then tab again )
(also since assignment said to take input file filename.mdt, it was not clear whether the input file would be named filename.mdt or if we had to take the name as an argument. I have taken the name as an argument (without the mdt as only filename was in italics) and then appended the .mdt part to it for input file, and .html for output

1. (---) is implemented as a dashed line ONLY when these are the only 3 characters on any line. This was done for more readability and so any random --- does not expand to a horizontal rule. also, no whitespace characters should be present before or after. (after this only the "\n" character must be there)

4. Codeblocks are implemented as according to markdown, so if an indented block follows a paragraph without a blank line in between, it will be considered to be a part of the paragraph instead of a codeblock. Hence a blankline separating codeblock from paragraphs are necessary

5. All escape characters are properly dealt with.

6. Errors and exceptions 
 - \* and \- need to be escaped. If any \* is found that does not correspond to a bold, italic or list form then an **AsterixNotMatched** Error is raised and also printed in the html file at the end.
 - if an open "\[" is found without escaping, then it throws a **bracketNotMatched** error if it doesnt close in the same line.
  in case it is supposed to be a link, then a "(.)" clause should follow right after the "\]" ends, otherwise it will be printed normally.
  error also thrown whenever a bold,italics,underline tag in a table cell isnt closed in that table cell itself.

7. codeblocks works fine for four spaces always BUT they should be 4 or more spaces or 1 or more tabs(if tabs get converted to spaces).In the assignment's .md file, the link "cse.iitd.ac.in" has unnatural non-printable spaces behind it, so it doesn't directly work but it does work after backspacing and adding normal spaces back again.
 
 If a codeblock is required to be placed inside a list, then the 4 spaces should be counted from 1+ the spaces behind the list item. so if a list item has 2 leading spaces, then in my list rules for a new lines the natural place to write is after 3 leading spaces, so for a codeblock you should use 7 spaces for nesting it inside this list item.


6. underlining is delimited by space. And an underscore before and after the text is necessary, also a space after the last underscore. 

2. Nested lists are a little weird as done by John Gruber. Its unclear how many spaces are needed for each nest. In markdown one space   would be enough to make a nested list (1st degree), but if you add 2 or 3 spaces before next  element it wouldn't nest another list and  make a 2nd degree list but instead be considered as part of the same 1st degree list. It takes 5 spaces for a 2nd degree nested list to  form in markdown. example is given as the next list, of which you can see the source of. 
 But the way I have implemented is as follows
 - the number of spaces behind the start of the numbering, denotes the number of nests, so if there was " 1." written at the start then it would be considered to be a nest inside the first list point above it that has fewer leading spaces. and "  1." would be nested inside the first point above it with lesser than 2 leading spaces and so on. this makes it easy to nest. 
 - each paragraph has a blank line in between, like original markdown. paragraphs appear when there are blank lines(more on that in next para)
 - all the text in the same point should start after (n+1) spaces from the left, if we are in the nth nested list. so for an unnested list, all the lines should begin with 1 space to be considered part of the list point, otherwise the list would be ended and a paragraph started.
 
 aside from these, if there is a blank line between two list points, then markdown wraps them around in <p> tags. My code handles this a bit differently and the <p> tags are placed at other places but the overall result is the same when the html file is ran.


3. example Markdown List to show the unclear number of leading spaces it takes for nesting multiple levels (but would give correctly 5 nests in my code)
 - 1space
  - 2space
   - 3space
    - 4space
     - 5space

7. for tables, it is mandatory for << and >> to be on a separate line and immediately be followed by a newline character. Bolding,underline,italics work well within each cell of the table, but html doesnt allow bolding to be done accross multiple cells with one tag, so the way I have implemented requires the \* or \*\* to end within the same cell

8. for email style quoting, all the ">>>...>" must be at the start of the line and without spaces. quotes can contain lists, paras, headers, tables etc, but do note that the start of the line would be assumed from just after the > ends, so for example if a header is present the # must be just after the >>..> without spaces
