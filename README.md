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
