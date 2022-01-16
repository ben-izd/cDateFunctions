# cDateFunctions

> Source code of DLL files will be in [another repository](https://github.com/ben-izd/cDateFunctions-Source).

> This repository also have [Wiki](https://github.com/ben-izd/cDateFunctions/wiki) with examples for some functions.

You can read more about their performance and capabilities in [Wolfram Community Post](https://community.wolfram.com/groups/-/m/t/2434669).

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

## Arithmetic Persian Leap Cycles Visualization

<img src="https://i.postimg.cc/Znzn198w/arithmetic-persian-33-leap-cycle.jpg" width="750" alt="Arithmetic Persian 33 Leap Cycle">

<img src="https://i.postimg.cc/Vv9v1Y4m/arithmetic-persian-128-leap-cycle.jpg" width="750" alt="Arithmetic Persian 128 Leap Cycle">

<img src="https://i.postimg.cc/bN8dgtNJ/arithmetic-persian-2820-1-leap-cycle.jpg" width="900" alt="Arithmetic Persian 2820 Leap Cycle">

<img src="https://i.postimg.cc/650qmpyh/arithmetic-persian-2820-2-leap-cycle.jpg" width="750" alt="Arithmetic Persian 2820 Leap Cycle">

<img src="https://i.postimg.cc/HWbVb2mR/arithmetic-persian-4166-leap-cycle.jpg" width="750" alt="Arithmetic Persian 4166 Leap Cycle">


You can read about their origins and inventors in [www.magiran.com/paper/2219482?lang=en](https://www.magiran.com/paper/2219482?lang=en).
