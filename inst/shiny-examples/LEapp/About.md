CyChecks is an examination of the pay patterns across Iowa State
University. While this salary data is publicly available through the
Iowa government
[website](https://data.iowa.gov/State-Finances/State-of-Iowa-Salary-Book/s3p7-wy6w),
it is not formatted in such a way as to allow users to compare pay
trends between genders. We've created CyChecks and the Shiny App to
allow users to more easily visualize this dataset. We feel this dataset
could be especially helpful to those looking for university jobs, who
might not know what the typical salary range is and therefore how to
negogiate an offer.

#### Problem Areas

While we strongly believe that CyChecks is allowing users to accurately
interact with publicly available salary information, but there are a
couple issues that we'd like to be completely transparant about.

1.  Department information is not included in the original, publicly
    available data file. Iowa State Human Resources gave us a list of
    all faculty and staff and their affiliated departments as of January
    1st 2019, however our dataset begins in 2007, therefore if a faculty
    or staff member left before 2019 then are not included in our Shiny
    App. To correct for this problem we have manually entered professor
    information, using faculty directories published by Iowa State each
    year. However, there are still cases in which a faculty member is
    not listed in the directory and their department affiliation is not
    readily identifiable. Unfortunately those cases continue to be left
    out of our Shiny App. We realize this method is non-reproducible,
    however it's the best methodology that we see at this time. If Iowa
    State University decides to publicly release directory information,
    we will make that publicly available.

2.  All of our salary estimates are based up the "total salary paid" for
    a given fiscal year. This is because total salary paid is always
    reported for all employees. However, this is not the same thing as
    the "base salary" for an employee. For instance, some professors are
    paid on 9-month appointments but will supplement their summer income
    with grant money. While their base salary may not change from year
    to year, their total salary paid can fluctuate, depending on grant
    funding availability.

<img src="hexsticker.png" width="25%" height="25%" />

Source code for this file can be found on our
[github](https://github.com/vanichols/CyChecks)
