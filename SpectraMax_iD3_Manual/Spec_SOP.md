# Standard Protocol for the SpectraMax iD3 Multi-Mode Microplate Reader

Original: 20190913  
Last Revised: 20200207

**Contents**  
[**Logging In**](#Logging_In)  
[**Troubleshooting**](#Troubleshooting)

<a name="Logging_In"></a> **Logging In**
1. Turn on the Spec by tapping the yellow power button (lower left corner of the screen platform) with your finger.  Initial startup will take a few minutes.
1. Select a User Profile to log in.  Either tap the Profile on the screen, or use an NFC pairing device set up for a particular Profile.
    1. Silbiger Lab: for general lab use; NFC Pairing magnet located on the right side of the instrument
    1. Admin: for any system changes are required: Admin Pin: 0000
1. Select a User Profile to log in
    1. Silbiger Lab
    1. Admin (Pin: 0000)
1. To create a new User Profile, log in with Admin, then select the Maintenance icon (wrench) on the bottom left toolbar, then select Users on the lefthand side.
    1. Select Create New User and type in the User name.
    1. Choose to set a pin or leave the option OFF.
    1. Choose to set an NFC Pairing or leave the option OFF.
    1. These settings can be changed at any time by returning to the Users page in the Admin account.

<a name="Troubleshooting"></a> **Troubleshooting**
1. For quick How-To videos, select the Maintenance icon (wrench) on the bottom left toolbar, then select Support.  Here you can watch videos on
    1. [Getting to konw youre new ID series reader](https://www.moleculardevices.com/en/assets/tutorials-videos/br/getting-to-know-your-new-spectramax-id3)
    1. [Personalizing your ID series reader](https://www.moleculardevices.com/en/assets/tutorials-videos/br/personalizing-your-spectramax-id3)
    1. [Starting a plate read](https://www.moleculardevices.com/en/assets/tutorials-videos/br/starting-plate-read-using-spectramax-id3-multi-mode)
    1. [Viewing results](https://www.moleculardevices.com/en/assets/tutorials-videos/br/viewing-results-on-spectramax-id3-multi-mode)
1. Lost connection between the instrument and the computer
    1. On the SpectraMax iD3 touchscreen, select the **Maintenance** icon (wrench), and go to **System Information**.
    1. Note the Assigned IP address.
    1. In the SoftMax Pro program, click the well plate icon in the upper lefthand corner to view the dropdown menu.
    1. Select **Options** on the bottom bar.
    1. Select **SpectraMax iDx Control** on the lefthand bar.
    1. In IP-Address 1, make sure the Assigned IP address shown on the instrument's screen is the same as one of the IP-Addresses shown on the iDx Control page.
    1. If unchecked, chek the box that "Enables control of SpectraMax iDx instruments"
1. Plate reader is connected but unresponsive through SoftMax Pro
    1. The lab computer is fairly slow, so be patient with it and try to not task overload.
    1. If the instrument is still unresponsive through the program after you've waited for possibly just slow task completion, try exiting the program and reopening.
    1. If the program will not let you exit because it says the instrument is running, go to the plate reader's touch screen and press "Disconnect".  Wait a moment (up to about a minute) on SoftMax Pro before attempting to reconnect to the instrument.
