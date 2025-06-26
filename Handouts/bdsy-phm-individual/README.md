# Public Health Modeling Project

## Starter Individual GitHub Repo

This is a starter GitHub repository is for students to copy as part of the 2025 Big Data Summer Immersion at Yale (BDSY) Public Health Modeling afternoon project.

Weekly material will be provided as `*.zip` files, which should get decompressed and the decompressed directory added to this main project directory. The purpose of this repository is to practice using Git and GitHub for individual projects, and to help students build a professional portfolio.

Please note that this repository is not intended for the group collaborative project.

You can find out more materials to support your Git/GitHub learning on the Canvas site from the first week of lectures and on our [Book of Workshops](https://ysph-dsde.github.io/Book-of-Workshops/) webpage.

During BDSY, you are welcome to attend one of our walk-in office hours Monday or Thursday from 12:30-1:30pm or register for an individual 30 minute appointment Tuesday or Wednesday from 12:30-1:30pm ([BDSY Bookings Page](https://outlook.office.com/book/BigDataSummerImmersionatYaleBDSYOfficeHours@yale.edu/?ismsaljsauthenabled)).

## About Repository

Lecture slides and `*.zip` files will be distributed through the BDSY Canvas site.

### Overview Of Contents

-   **R Project:** `bdsy-phm-individual.Rproj`
-   **Datasets:** Imported via a GitHub raw URL from the instructor GitHub page.
-   **Week 2:** The worked-through `.qmd` illustrating how to run a time series analysis. Images, references, and reference formatting is included so the document renders correctly.
-   `.gitignore`: Directs Git to ignore certain files that are not necessary for version control tracking or GitHub distribution.
-   `renv()` version: 1.0.11. This is included to reproduce the environment.

## Using this Repository

### No Version Control

We recommend that you use version control with git for all of your projects. If you do not wish to do this, you can save the entire directory in any subfolder you wish to house the project. All of the code should still work so long you initialize the environment correctly. See **Initializing the Environment** below.

### Adding Version Control with git

The following steps assume that you have installed git on your computer and you have a command-line application (i.e. Terminal for Macs and Windows Terminal for windows).

1.  Move the project directory (the entire folder originally downloaded as a \*.zip file) to the location you want to store it.

2.  Open the command-line application and navigate into the project directory.

    ```         
    cd "/file_location/project_directory_name"
    ```

3.  Initialize git.

    ```         
    git init
    ```

**NOTE:** We have already included a `.gitignore` to the directory listing the types of files you do not need git to version control or share with GitHub.

### Adding to GitHub

These directions assume you have initialized git in your project folder. If you have not done this, you will need to follow the steps listed under **Adding Version Control with git**.

1.  [Create a new](https://github.com/new) GitHub repository ([Further documentation](https://docs.github.com/en/repositories/creating-and-managing-repositories/creating-a-new-repository)).

    **NOTE:** Do not use a template or include a description, README file, `.gitignore`, or license. Only adjust the GitHub account owner as needed and create the name for the new repository. We recommend initially setting the repository to Private.

2.  Open the command-line application and navigate into the project directory.

    ```         
    cd "/file_location/project_directory_name"
    ```

3.  Confirm that git has been initialized by checking the project status. If the project is initialized you should see that git is present and is tracking files.

    ```         
    git status
    ```

4.  If `git status` shows untracked files or untracked edits for tracked files, then add and commit these to git. `git add .    git commit -m "first commit"`

5.  Create a branch called `main`.

    ```         
    # check branch main does not exist
    git branch -a

    # if no branches add one called main
    git branch -M main
    ```

6.  Assign the GitHub repository location and save this location to the proxy name `origin`. The file transfer protocol can either by SSH or HTML, depending on your git configurations. Change the location used below by copying it from the empty GitHub repository you just created.

    ```         
    # if using SSH
    git remote add origin git@github.com:EXAMPLE-USER/NEW-REPOSITORY.git

    # or if using HTTPS
    git remote add origin https://github.com/EXAMPLE-USER/NEW-REPOSITORY.git
    ```

7.  Finally push the directory to your empty GitHub repository.

    ```         
    git push -u origin main
    ```

### Initializing the Environment

1.  Open the project directory on your local device.

2.  Launch the project by opening `Introduction to R and Tidyverse.Rproj`.

3.  In the R console, activate the environment by running:

    ```         
    renv::init()               # initialize the project
    renv::restore()            # download packages and their version saved in the lockfile.
    ```

    **NOTE:** If you are asked to update packages, say no. The `renv()` is intended to recreate the same environment under which the project was created, making it reproducible. You are ready to proceed when running `renv::restore()` gives the output `- The library is already synchronized with the lockfile.`. You can read more about `renv()` in their [vignette](https://rstudio.github.io/renv/articles/renv.html).
