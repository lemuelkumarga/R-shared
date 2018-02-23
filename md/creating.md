## Creating A New Repository from the Template

To create a new project by using this template as skeleton, we need to manually "fork" this template. Extra precaution is also required as the R-template contains links to other repos (submodules).

Please replicate the steps below in Terminal to ensure success.

``` sh
# First: Create an empty repository in github via https://github.com/new

# Clone the newly created empty repo
git clone https://github.com/<username>/<new_repo_project_name>

# Define this clone as a fork of R-template
cd <new_repo_project_name> 
git remote add upstream https://github.com/lemuelkumarga/R-template.git

# Pull all the files from the template
git pull upstream master

# Remove the "Create New Repo from Template section"
sed -i 's/, prepend_mds=c("shared\/md\/requirements.md","shared\/md\/creating.md","shared\/md\/cloning.md")//g' main.Rmd

# Initialize submodules
git submodule init
git submodule update

# When cloned, submodules are detached from the HEAD. We attempt to rectify this issue to prevent problems in git
cd shared
git checkout -b tmp
git checkout master
git merge tmp
git branch -d tmp
cd ..

# Push your changes to the cloud
git push -u origin master

# Return to original folder if desired
cd ..
```

