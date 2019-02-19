-------------------------------------------------------------------------------
                               W  R  Y
                                COBOL
-------------------------------------------------------------------------------

By: Erik Eriksen
Created: 12/16/18-2/20/19 (on and off)
GitHub Project: https://github.com/shamrice/wry-cobol/

Description:
------------
Wry COBOL is a version of an old Qbasic game I created back in 1999-2000 by
the same name (without the COBOL part). It's a text 'adventure' game where you
select which option you would like to move the story forward. There are
four episodes to choose from and each one has it's own story that's not related
to the other episodes.

How it works:
-------------
The wry-cobol application reads from the 'data' directory where '.dat' files
are stored which contains different data files for the story page choices,
story page text and overall story information. This data is read and parsed
in the application on the fly as the user selects their choices.

The data files were generated using my wry-parser Java application I created
which reads the original Wry.bas Qbasic source file. This parsing application
reads, parses and links each episodes story into useful objects that can be
written out to different types of ouput. One of those outputs was created and
used here is the one I made to output to the data files used for this
application. If you're interested in seeing the source file (or even running
it yourself) it can be found at: https://github.com/shamrice/wry-parser

So, what you have here is a Qbasic source file that was parsed by a Java
application to output data to be read by a COBOL application. :)

Hope you enjoy!
