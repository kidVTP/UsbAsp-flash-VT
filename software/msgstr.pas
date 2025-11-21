unit msgstr;

{$mode objfpc}{$H+}

interface

resourcestring
  STR_CHECK_SETTINGS     = 'Check settings';
  STR_READING_FLASH      = 'Reading memory...';
  STR_WRITING_FLASH      = 'Programming memory...';
  STR_WRITING_FLASH_WCHK = 'Programming memory (with verification)...';
  STR_CONNECTION_ERROR   = 'Error connecting ';
  STR_SET_SPEED_ERROR    = 'Error setting SPI speed';
  STR_WRONG_BYTES_READ   = 'Number of bytes read not equal to IC size';
  STR_WRONG_BYTES_WRITE  = 'Number of bytes written not equal to IC size';
  STR_WRONG_FILE_SIZE    = 'File size larger than IC size';
  STR_ERASING_FLASH      = 'Erasing memory...';
  STR_DONE               = 'Done';
  STR_BLOCK_EN           = 'Possible protection is enabled. Press \"Unprotect\" and check datasheet "Снять защиту" и сверьтесь с даташитом';
  STR_VERIFY_ERROR       = 'Verification error on address: ';
  STR_VERIFY             = 'Verifying memory...';
  STR_TIME               = 'Execution time: ';
  STR_USER_CANCEL        = 'Cancelled by user';
  STR_NO_EEPROM_SUPPORT  = 'This firmware does not support memory programming';
  STR_MINI_EEPROM_SUPPORT= 'This firmware does not support I2C and MW!';
  STR_I2C_NO_ANSWER      = 'IC not responding';
  STR_COMBO_WARN         = 'IC will be erased and programmed. Continue?';
  STR_SEARCH_HEX         = 'Search HEX value';
  STR_GOTO_ADDR          = 'Goto address';
  STR_NEW_SREG           = 'New SREG: ';
  STR_OLD_SREG           = 'Old SREG: ';
  STR_START_WRITE        = 'Begin programming?';
  STR_START_ERASE        = 'Erase IC?';
  STR_45PAGE_STD         = 'Default page size is set';
  STR_45PAGE_POWEROF2    = 'Page size is power of 2';
  STR_ID_UNKNOWN         = '(Unknown)';
  STR_SPECIFY_HEX        = 'Use HEX numbers';
  STR_NOT_FOUND_HEX      = 'Value not found';
  STR_USB_TIMEOUT        = 'USB_control_msg timeout!';
  STR_SIZE               = 'Size: ';
  STR_CHANGED            = 'Modified';
  STR_CURR_HW            = 'Current programmer: ';
  STR_USING_SCRIPT       = 'Using script: ';
  STR_DLG_SAVEFILE       = 'Save changes?';
  STR_DLG_FILECHGD       = 'File modified';
  STR_SCRIPT_NO_SECTION  = 'There is no section: ';
  STR_SCRIPT_SEL_SECTION = 'Select section';
  STR_SCRIPT_RUN_SECTION = 'Runs section: ';
  STR_ERASE_NOTICE       = 'The process can take more than a minute on large flash!';

implementation

end.

