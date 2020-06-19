# Git process used

**Printing working repo and list files in current dir**

$ pwd
$ ls

**Cloning assignment repo**

$ cd ~/Documents/info201

$ git clone "https://github.com/info-201a-sp20/a1-uamoazzam.git" 

$ cd a1-uamoazzam

**Creating image folder**

$ mkdir imgs

$ cd imgs

$ curl https://sunbeamwhdh.files.wordpress.com/2020/03/200330_fauci.jpg > maskimg.png

**Checking to make sure both files were created**

$ cd ..

$ ls

**Add text to README.md, other-questions.md**

**Commit changes to GitHub**

$ git add .

$ git status

$ git commit m- "Adding finished README article, other-questions file, and 
image folder"

$ git status

$ git push

--- 

# Cheat sheets used

[Markdown Cheat Sheet](https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet): This cheat sheet, which was given to me by my TA, served as a good quick reference for me for minor syntax such as linking an image to the markdown, making bullet points, etc. Its also really organized, and had a hekpful hyperlinked table of contents, which made it so I could click on a topic and go straight to it rather than having to look for it.

[Git Cheat Sheet](https://education.github.com/git-cheat-sheet-education.pdf): This cheat sheet, which I got from GitHub Education provided a good reference for the most important and commonly used git commands, even having information on how to download and set up user configuration on Git. I was able to use this cheat sheet as a reminder for how to download files using curl, how to clone repos, etc.

