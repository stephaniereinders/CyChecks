While we strongly believe that CyChecks is allowing users to accurately
interact with publicly available salary information there are a couple
issues that we'd like to be completely transparant about.

1.  Department information is not included in the original, publicly
    available data file. Iowa State Human Resources gave us a list of
    all faculty and staff and their affiliated departments as of January
    1st 2019, however our dataset begins in 2007, therefore if
    faculty/staff left before that time period then are not included in
    our Shiny App. To correct for this problem we have manually entered
    professor information, using faculty directories published by Iowa
    State each year. However, there are still cases in which a faculty
    member is not listed in the directory and their department
    affiliation is not readily identifiable. Unfortunately those cases
    continue to be left out of our Shiny App. We realize this method is
    non-reproducible, however it's the best methodology that we see at
    this time. If Iowa State University decides to publicly release
    directory information, we will make that publicly available.

2.  All of our salary estimates are based up the total salary paid for a
    given fiscal year. This is because total salary paid is always
    reported for all employees. However, this is not the same thing as
    the base salary for an employee.
