# cDateFunctions
Alternative to part of Wolfram Language built-in date calculation functions.

This page is under construction.

How to use this Package:
- For the normal cDateFunctions (those that were written in pure wolfram-language), download and run [cDateFunctions.wl](https://github.com/ben-izd/cDateFunctions/blob/main/cDateFunctions.wl)
- For the Functions use `LibraryLink`, you'll need [Main DLL File](https://github.com/ben-izd/cDateFunctions/blob/main/LibraryLink/cDateFunctionsLibraryLink.dll) and [Interface DLL File](https://github.com/ben-izd/cDateFunctions/blob/main/LibraryLink/cDateFunctionsLibraryLinkInterface.dll) and [Library File](https://github.com/ben-izd/cDateFunctions/blob/main/LibraryLink/cDateFunctionsLibraryLinkInterface.wl)
    - In Mathematica, you should set the DLL paths before running the Library File:
      ```
      cDateFunctions`LibraryLink`Path`DLL (* set to Main DLL File Path *);
      cDateFunctions`LibraryLink`Path`InterfaceDLL (* set to Interface DLL File Path *);
      ```
    - Run Library File.

## Availabe methods for Calendars:

### ArithmeticPersian
| Method Name | Author | Pattern Duration |
| --- | --- | --- |
| "Cycle-33" | Omar Khayyam | 33 |
| "Cycle-128" | Hassan Taghi Zade | 128 |
| "Cycle-2820-1" | Zabih Allah Behrouz | 2820 |
| "Cycle-2820-2" | Musa Akrami  | 2820 |
| "Cycle-4166" | Ahmad Farmad | 4166 |

Source : [www.magiran.com/paper/2219482?lang=en](https://www.magiran.com/paper/2219482?lang=en)

### Islamic
| Method Name | Leap Years |
| --- | --- |
| "15-Based" | 2, 5, 7, 10, 13, 15, 18, 21, 24, 26, 29 |
| "16-Based" | 2, 5, 7, 10, 13, 16, 18, 21, 24, 26, 29 |
| "Indian" | 2, 5, 8, 10, 13, 16, 19, 21, 24, 27, 29 |
| "Habash al-Hasib" | 2, 5, 8, 11, 13, 16, 19, 21, 24, 27, 30 |


