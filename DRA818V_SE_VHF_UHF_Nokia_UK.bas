'Copyright AX elektronika doo, June 2015
'version for Nokia 5510
'scanning disabled, but in principle it works, scanning stops on the channel with a signal.
'all commands are done with encoder and button.
'short press on encoder button changes band from VHF to UHF (bot for simplex and repeaters)
'long press jumps to next menu
'menus are: Simplex channels, Repeaters, Echolink, Free freq. setting, Setup (for Squelch, Volume and Microphone level)
'Mic. level was not tested.
'Free freq. is limited from 134 MHz to 490 MHz, freq. bands in between are not limited, use at your own responsibility
'still in development


$hwstack = 128
$swstack = 128
$framesize = 128
$regfile = "m328def.dat"
'$regfile = "m168pdef.dat"
'$regfile = "m32def.dat"
$crystal = 1000000                                          'set fuse bits to 8 MHZ internal RC and enable Division with 8

$baud = 9600
Dim Bw As Bit                                               ' bandwith in KHz ( 0= 12.5KHz or 1= 25KHz )
Bw = 0                                                      '12.5 kHz channel_s

'*******************************************************************************
$lib "glcd-Nokia3310.lib"                                   'put that lib to Bascom-AVR/Lib directory

Config Graphlcd = 128x64sed , Rst = Portc.4 , Cs1 = Portc.3 , A0 = Portc.2 , Si = Portc.0 , Sclk = Portc.1

'CE=CS1
'SCLK=CLK
'DC=A0
'DIN=SI

'CS may be connected to GND

' Rst & Cs1 is optional

'Const Negative_lcd = 1                                      'Inverting screen
'Const Rotate_lcd = 1                                        'Rotate screen to 180°


'Dim I As Byte , N As Word , M As Byte
'Dim Text As String * 8

'***************************** Program *****************************************
Initlcd
Cls
Glcdcmd 33 : Glcdcmd 190                                    'Normal Contrast


'-------------------------------------------------------------------------------------------------------------
'2m band
Dim Ftx As Single                                           '
Dim Ftx1 As Single                                          '

Ftx = 1452000                                               'tx frequency in MHz (134.0000 - 174.0000)
Dim Frx As Single                                           'aux freq. simplex channels
Dim Frx1 As Single                                          'freq. simplex channels that is written into DRA module Frx1=Frx/10000
Frx = 1452000                                               'Rx Frequency In MHz(134.0000 - 174.0000)

Dim Ftx_r As Single                                         'same principle as above
Dim Ftx_r1 As Single                                        '
Dim Frx_r As Single
Dim Frx_r1 As Single
Ftx_r = 1450000                                             ' tx Frequency In MHz(134.0000 - 174.0000)
Frx_r = 1456000                                             ' rx frequency in MHz (134.0000 - 174.0000)

Dim Frtx_e As Single                                        'echolink freq
Dim Frtx_e1 As Single

Frtx_e = 1445500
'Frtx_e1 = 144550

Dim Ftx_free As Single                                      'free freq. set
Dim Ftx1_free As Single                                     '
Ftx_free = 1440000
'-------------------------------------------------------------------------------------------------------------

'70cm band
Dim Ftx_u As Single                                         '
Dim Ftx1_u As Single                                        '

Ftx_u = 4334000                                             'tx frequency in MHz (134.0000 - 174.0000)
Dim Frx_u As Single                                         'aux. freq. simplex
Dim Frx1_u As Single                                        'freq. simplex channels that is written into DRA module Frx1_u=Frx_u/10000
Frx_u = 4334000                                             'Rx Frequency In MHz(134.0000 - 174.0000)

Dim Ftx_r_u As Single                                       'same as above
Dim Ftx_r1_u As Single                                      'repeater channels
Dim Frx_r_u As Single
Dim Frx_r1_u As Single

Ftx_r_u = 4309500                                           ' tx Frequency In kHz repeater
Frx_r_u = 4385500                                           ' rx frequency in kHz
Ftx_r1_u = Ftx_r_u / 10000
Frx_r1_u = Frx_r_u / 10000

Dim Ftx_free_u As Single                                    'free freq. set
'Dim Ftx1_free_u As Single                                   '
Ftx_free_u = 4334000

Ftx1_u = Ftx_u / 10000
Frx1_u = Frx_u / 10000

Dim Set_band As Bit                                         '=0>UHF; =1>VHF
Set_band = 0

'-------------------------------------------------------------------------------------------------------------
Dim Izhod As Bit                                            '=0 not exit with a long press encoder
Izhod = 0

Dim Tx_ctcss As String * 4
Tx_ctcss = "0000"                                           ' Ctcss Frequency(0000 - 0038 ) ; 0000 = "no CTCSS"
Dim Rx_ctcss As String * 4
Rx_ctcss = "0000"                                           'ctcss frequency ( 0000 - 0038 ); 0000 = "no CTCSS"
Dim Squ_e As Eram Byte
Dim Squ As Byte

Squ = Squ_e
Squ = 1                                                     'squelch level ( 0 - 8 ); 0 = "open"
Dim Channel_su As Word                                      'simplex channel UHF
Channel_su = 272
Dim Channel_v As Byte                                       'simplex VHF channels
Channel_v = 16

Dim Channel_rv As Byte                                      'repeater VHF channels
Channel_rv = 48

Dim Channel_ru As Word                                      'repeater UHF channels
Channel_ru = 684

Dim Channel_echo As Byte                                    'repeater ECHOlink channels
Channel_echo = 1

Dim Mod_level As Byte
Mod_level = 8

Dim Volume_e As Eram Byte
Dim Volume As Byte
Volume = Volume_e

Dim Tx_bit As Bit
Tx_bit = 0                                                  'Tx_bit=0 means tranceiver receives

                                              'Hz
Tx_led Alias Portd.5
Config Tx_led = Output

Rx_led Alias Portd.6
Config Rx_led = Output


Dim Pojdi As Bit                                            'acknowledge on Encoder

Bipbip Alias Portb.0                                        'Beeper output
Config Bipbip = Output
Bipbip = 1                                                  'no beeping

'Dim Y0 As Dword

Dim Freq_incr As Dword , Step_freq As Single                ', Freq1 As Single       ', Step_freq1 As Dword
'Step_freq = 0.0125
Step_freq = 125

Freq_incr = 0

Dim Opis_v As String * 16                                   'VHF repeater names
Dim Opis_u As String * 16                                   'UHF repeater names


Dim Add_rx As Bit                                           'additional reception on 400 MHz, no TX however
Add_rx = 0                                                  '=0, no additional reception; =1 additional reception enabled

Dim C1 As Byte                                              'Menu select
Dim C2 As Byte                                              'menu for step freq
Dim Setupbyte As Byte                                       'setup menu select
Setupbyte = 1

C2 = 1

Config Single = Scientific , Digits = 4

C1 = 1

Dim Pomoznibit As Bit                                       '
Pomoznibit = 0                                              '=0 counting up, =1 counting down
'Dim C5 As Byte                                              'c5 determines what configured with encoder: C5=0 nastavljam Shape,C5=1 nastavljam freq
Dim Change As Bit

Dim Pojdi_up As Bit                                         '   pojdi=1 nastavljen parameter is taken into account
Dim Pojdi_down As Bit

Dim Print_once As Bit                                       '=1 sends data on UART, =0 does not send UART
Print_once = 0

'Dim Num As Byte , Num1 As Byte

'Dim S1 As Byte

Dim I1 As Byte
I1 = 0

'in case you use 2x16 LCD this is section to select it

'Config Lcdbus = 4
'Config Lcd = 16x2
'Config Lcd = 16 * 2

Const Set_board = 1                                         'MegaPin=0, VHF/UHF board =1
'(
#if Set_board = 0                                           'MegaPin
Config Lcdpin = Pin , Db4 = Portc.4 , Db5 = Portc.5 , Db6 = Portc.6 , Db7 = Portc.7 , E = Portc.2 , Rs = Portc.1       'prikaz v MegaPin
#else
Config Lcdpin = Pin , Db4 = Portc.2 , Db5 = Portc.3 , Db6 = Portc.4 , Db7 = Portc.5 , E = Portc.0 , Rs = Portc.1       'prikaz v vezju VHF
#endif
')
Dim M As Byte
'Config Debounce = 50
Cursor Off
'Showpic 0 , 0 , Okvir

Setfont Font10x16tt
'Setfont Font6x8
Lcdat 1 , 10 , "2m TRX"
Setfont Font6x8
Lcdat 4 , 1 , "AX elektronika"
For M = 175 To 190                                          'slow turn-on on LCD
   Glcdcmd 33 : Glcdcmd M
   Waitms 200
Next
Glcdcmd 33 : Glcdcmd 190
Wait 2

Initlcd
Cls
Glcdcmd 33 : Glcdcmd 190                                    'Normal Contrast
Gosub Biper_s
'Wait 2


Enk1 Alias Pind.3                                           'set encoder pins
Enk2 Alias Pind.2
Enk3 Alias Pind.4

Config Enk1 = Input
Config Enk2 = Input
Config Enk3 = Input
Portd.4 = 1
Portd.2 = 1
Portd.3 = 1

Rx_pin Alias Pinb.2                                         'Squelch output from DRA818
Config Rx_pin = Input                                       '0=receiving a signal, 1=no signal on receive
Rx_pin = 1


Dim Krog As Bit                                             '=0,first time around Do-Loop; =1 second time around Do-Loop
Krog = 0

Dim Clock1 As Word , Stevec As Byte , Scan_bit As Bit       'for timer
Scan_bit = 1                                                '=0: no scanning; =1: scanning

Dim Freq_scan As Bit                                        '=0: no scanning; =1: scanning
Freq_scan = 0

Dim Set_band_pom As Bit                                     'aux. variable that holds info which band was used last


Ptt Alias Pinb.4
Config Ptt = Input
Ptt = 1

Dim Ser_input As String * 15

Config Timer0 = Timer , Prescale = 1

Dim B2 As Byte
Enable Interrupts

Config Int0 = Falling
Config Int1 = Falling


Enable Int1
Enable Int0
Pojdi_up = 1                                                'Simplex ch was changed
Pojdi_down = 1                                              'Simplex ch was changed


On Int0 Enkoder_sub1
On Int1 Enkoder_sub2

'timer not used

'On Timer0 Tim0_isr
'Enable Timer0
'Start Timer0


'Deflcdchar 0 , 12 , 18 , 18 , 12 , 4 , 5 , 6 , 5            ' OK znak
'Deflcdchar 1 , 17 , 10 , 4 , 10 , 17 , 32 , 32 , 32         ' not OK znak

Cls
'Gosub Oled$clear

Frx1 = Frx / 10000
Ftx1 = Ftx / 10000
Frx_r1 = Frx_r / 10000
Ftx_r1 = Ftx_r / 10000
Ftx1_free = Ftx_free / 10000

'(
If C1 = 1 Then Print "AT+DMOSETGROUP=" ; 0 ; "," ; Ftx1 ; "," ; Frx1 ; "," ; Tx_ctcss ; "," ; Squ ; "," ; Rx_ctcss       '; "," ; 0       ' simplex
If C1 = 2 Then Print "AT+DMOSETGROUP=" ; 0 ; "," ; Ftx_r1 ; "," ; Frx_r1 ; "," ; Tx_ctcss ; "," ; Squ ; "," ; Rx_ctcss       ' ; "," ; 0       ' repeaters
If C1 = 3 Then Print "AT+DMOSETGROUP=" ; 0 ; "," ; Frtx_e1 ; "," ; Frtx_e1 ; "," ; Tx_ctcss ; "," ; Squ ; "," ; Rx_ctcss       ' ; "," ; 0       ' Echolink
')

'enable wake up from Powerdown with encoder or encoder button
Pcicr.2 = 1
Pcmsk2 = &B00011100

'wake-up from powerdown with PTT
Pcicr.0 = 1
Pcmsk0.4 = 1                                                'na PCINT4

Print_once = 1                                              'at powerup send data to DRA module

Dim Wdbit As Bit
Dim Bwd As Byte

'WD not used
'Config Watchdog = 512
'Start Watchdog
'Mcusr.3 = 0
'Wdtcsr.6 = 1                                                'enable WD interrupt
'Wdtcsr.4 = 1
'Wdtcsr.3 = 0
'Start Watchdog
'Config Portb.1 = Output

Do
'Portb.1 = Not Portb.1
'Bwd = Peek(0)
   If Krog = 1 Then                                         ' there was a WD overflow
      Wdbit = 1
      Else
      Wdbit = 0                                             'store the flag
   End If


'Mcusr.3 = 0
'Wdtcsr.6 = 1                                                'enable WD interrupt
'Wdtcsr.4 = 1
'Wdtcsr.3 = 0



Pojdi = 0

Gosub Ledice_sub

If Ptt = 1 And Tx_bit = 1 Then                              'needed to enable correct data sending to DRA818
   Tx_bit = 0
   Waitms 500
   Print_once = 1
End If


  ' If Bw = 1 Then
  '    Step_freq = 250
  '    Else
  '    Step_freq = 125
  ' End If

   Debounce Enk3 , 0 , Povecaj_c , Sub                      'enkoder button

   If C1 = 1 Then                                           'simplex channels
      'Stevec = 4
      Do
'this part of the code enables scanning of two VHF/UHF channels
'(
         Set_band_pom = Set_band
         If Squ > 0 Then                                    'added to work also at fully open Squelch
            While Stevec = 4
               Debounce Enk3 , 0 , Set_freq_sub , Sub          'encoder button to select menu
               Set_band = Not Set_band
               Gosub Nastavi_simpleks
               Waitms 200
               If Rx_pin = 0 Then
                  Stevec = 0
                  Stop Timer0
                  Exit While
               End If
               If Ptt = 0 Then
                  Stevec = 0
                  'Set_band = Set_band_pom                      'last used band
                  Scan_bit = 0
                  Exit While                                   'in case of Tx exit While
               End If
            Wend
         End If
         Scan_bit = 1

         If Stevec < 4 Then
            Debounce Enk3 , 0 , Start_timer_sub , Sub       'encoder button to select menu
            Else
            Debounce Enk3 , 0 , Set_freq_sub , Sub          'encoder button to select menu
         End If
')
'do sem
         Debounce Enk3 , 0 , Set_freq_sub , Sub             'encoder button to select menu** to gre ven pri skeniranju

         Gosub Ledice_sub
         If Ptt = 1 And Tx_bit = 1 Then                     'needed to enable correct data sending to DRA818
            Tx_bit = 0
            Waitms 500
            Print_once = 1
         End If


         Setfont Font10x16tt                                '13x12_segoe                                'Digital10x16                               'Font11x14a                                 'My12_16

         'Lcdat 1 , 10 , " Simpleks " , 1                    '

         If Set_band = 0 Then                               '=0 displays VHF, =1 displays UHF simpleks ch
            Lcdat 1 , 1 , Frx1                              ' ; " MHz"
            Setfont Font6x8
            Disable Interrupts
            Lcdat 3 , 1 , "CH " ; Channel_v ; "         "   'simplex
            If Stevec = 4 Then
               Lcdat 4 , 1 , "Scanning"                     'simplex
               Else
               Lcdat 4 , 1 , "         "                    'simplex
            End If
            Enable Interrupts
            Else
            Setfont Font10x16tt
            Lcdat 1 , 1 , Frx1_u
            Setfont Font6x8
            Disable Interrupts
            Lcdat 3 , 1 , "CH " ; Channel_su ; "         "  'simplex
            If Stevec = 4 Then
               Lcdat 4 , 1 , "Scanning"                     'simplex
               Else
               Lcdat 4 , 1 , "         "                    'simplex
            End If
            Enable Interrupts
         End If
         Setfont Font6x8
         If Set_band = 1 Then                               'display non.active freq (bottom part of LCD)
            Disable Interrupts
            Lcdat 5 , 1 , Frx1 ; " MHz"
            Lcdat 6 , 1 , "CH " ; Channel_v ; "         "   'simplex
            Enable Interrupts
            Else
            Disable Interrupts
            Lcdat 5 , 1 , Frx1_u ; " MHz"
            Lcdat 6 , 1 , "CH " ; Channel_su ; "         "  'simplex
            Enable Interrupts
         End If
         If Print_once = 1 Then
            Print_once = 0
            Gosub Nastavi_simpleks
         End If

         Print                                              '"AT+DMOVERQ"                                       'print is needed because we use Powerdown
         If Krog = 1 Then                                   ' And Scan_bit = 0 Then
            'Config Powermode = Powerdown                    'it turns off TX LED
            Sleep
         End If
      '   Print
         Krog = Not Krog


      Loop Until Izhod = 1
      Cls
      Izhod = 0
      C1 = 2
      Krog = 0                                              'it should be here or else it goes immediatelly to powerdown
   End If


   If C1 = 2 Then                                           'repeater channels
      Do
         Gosub Ledice_sub

         Select Case Channel_rv                             'VHF repeaters
            Case 47 : Opis_v = "MALIC  "
            Case 48 : Opis_v = "       "
            Case 49 : Opis_v = "MOHOR  "
            Case 50 : Opis_v = "NANOS  "
            Case 51 : Opis_v = "SLEMENE"
            Case 52 : Opis_v = "TRDINOV"
            Case 53 : Opis_v = "       "
            Case 54 : Opis_v = "KORADA "
            Case 55 : Opis_v = "ZAGARSK"
            Case 56 : Opis_v = "MRZLICA"
            Case 57 : Opis_v = "       "
            Case 58 : Opis_v = "POHORJE"
            Case 59 : Opis_v = "       "
            Case 60 : Opis_v = "GRMADA "
            Case 61 : Opis_v = "IZOLA  "
            Case 62 : Opis_v = "KRIM   "
            Case 63 : Opis_v = "KANIN  "
         End Select

         Select Case Channel_ru                             'UHF repeaters
            Case 684 : Opis_u = "ZAGARSK"
            Case 685 : Opis_u = "       "
            Case 686 : Opis_u = "       "
            Case 687 : Opis_u = "       "
            Case 688 : Opis_u = "MIRNA G"
            Case 689 : Opis_u = "       "
            Case 690 : Opis_u = "S. SLEM"
            Case 691 : Opis_u = "       "
            Case 692 : Opis_u = "       "
            Case 693 : Opis_u = "       "
            Case 694 : Opis_u = "KRVAVEC"
            Case 695 : Opis_u = "       "
            Case 696 : Opis_u = "GRMADA "
            Case 697 : Opis_u = "       "
            Case 698 : Opis_u = "BREZICE"
            Case 699 : Opis_u = "       "
            Case 700 : Opis_u = "TRSTELJ"
            Case 701 : Opis_u = "       "
            Case 702 : Opis_u = "       "
            Case 703 : Opis_u = "       "
            Case 704 : Opis_u = "PIRAN  "
            Case 705 : Opis_u = "       "
            Case 706 : Opis_u = "BOC    "
            Case 707 : Opis_u = "       "
            Case 708 : Opis_u = "       "
            Case 709 : Opis_u = "       "
            Case 710 : Opis_u = "JANCE  "
            Case 711 : Opis_u = "       "
            Case 712 : Opis_u = "SV. ROK"
            Case 713 : Opis_u = "       "
            Case 714 : Opis_u = "KRVAVEC"
            Case 715 : Opis_u = "       "
            Case 716 : Opis_u = "CE MEST"
            Case 717 : Opis_u = "       "
            Case 718 : Opis_u = "       "
            Case 719 : Opis_u = "       "
            Case 720 : Opis_u = "LAZE   "
            Case 721 : Opis_u = "       "
            Case 722 : Opis_u = "       "
            Case 723 : Opis_u = "       "
            Case 724 : Opis_u = "       "
            Case 725 : Opis_u = "       "
            Case 726 : Opis_u = "SV. ROK"
            Case 727 : Opis_u = "       "
            Case 728 : Opis_u = "       "
            Case 729 : Opis_u = "       "
            Case 730 : Opis_u = "KUM    "
            Case 731 : Opis_u = "       "
            Case 732 : Opis_u = "       "
            Case 733 : Opis_u = "       "
            Case 734 : Opis_u = "S.PLANI"
            Case 735 : Opis_u = "       "
            Case 736 : Opis_u = "POHORJE"
            Case 737 : Opis_u = "       "
            Case 738 : Opis_u = "POHORJE"
            Case 739 : Opis_u = "       "
            Case 740 : Opis_u = "       "
            Case 741 : Opis_u = "       "
            Case 742 : Opis_u = "MRZLICA"
            Case 743 : Opis_u = "       "
            Case 744 : Opis_u = "MALIC  "
            Case 745 : Opis_u = "       "
            Case 746 : Opis_u = "STRMEC "
            Case 747 : Opis_u = "       "
            Case 748 : Opis_u = "       "
            Case 749 : Opis_u = "       "
            Case 750 : Opis_u = "URSLJA "
         End Select

'part of code that scans two repeater channels
'(
         Set_band_pom = Set_band
         If Squ > 0 Then                                    '
            While Stevec = 4
               Debounce Enk3 , 0 , Set_freq_sub , Sub          'encoder button to select menu
               Set_band = Not Set_band
               Gosub Nastavi_repetitor
               Waitms 200
               If Rx_pin = 0 Then
                  Stevec = 0
                  Stop Timer0
                  Exit While
               End If
               If Ptt = 0 Then
                  Stevec = 0
                  'Set_band = Set_band_pom
                  Scan_bit = 0
                  Exit While                                   'at TX exit while
               End If
            Wend
         End If
         Scan_bit = 1

          If Stevec < 4 Then
            Debounce Enk3 , 0 , Start_timer_sub , Sub       'encoder button to select menu
            Else
            Debounce Enk3 , 0 , Set_freq_sub , Sub          'encoder button to select menu
         End If
')
'do sem

         Debounce Enk3 , 0 , Set_freq_sub , Sub             'encoder button to select menu

         If Ptt = 1 Then

               If Set_band = 0 Then                         'select band
                  Setfont Font10x16tt                       'display active freq (upper pard of LCD)
                  Lcdat 1 , 1 , Frx_r1 ; " "
                  Setfont Font6x8
                  Disable Interrupts                        'it should be there or else LCD freezes
                  Lcdat 3 , 1 , "CH " ; Channel_rv ; " " ; Opis_v ; "   "       'VHF repeater
                  If Stevec = 4 Then
                     Lcdat 4 , 1 , "Scanning"
                     Else
                     Lcdat 4 , 1 , "         "
                  End If
                  Enable Interrupts
                  Else
                  Setfont Font10x16tt
                  Lcdat 1 , 1 , Frx_r1_u ; "   "
                  Setfont Font6x8
                  Disable Interrupts
                  If Stevec = 4 Then
                     Lcdat 4 , 1 , "Scanning"
                     Else
                     Lcdat 4 , 1 , "         "
                  End If
                  Lcdat 3 , 1 , "CH " ; Channel_ru ; " " ; Opis_u ; "   "       'UHF repeater
                  Enable Interrupts
               End If
               If Set_band = 1 Then                         'display non active freq.
                  Setfont Font6x8
                  Lcdat 5 , 1 , Frx_r1 ; " MHz"
                  Disable Interrupts
                  Lcdat 6 , 1 , "CH " ; Channel_rv ; " " ; Opis_v ; "   "       'UHF repeaters  RX
                  Enable Interrupts
                  Else
                  Setfont Font6x8
                  Lcdat 5 , 1 , Frx_r1_u ; " MHz"
                  Disable Interrupts
                  Lcdat 6 , 1 , "CH " ; Channel_ru ; " " ; Opis_u ; "   "       'UHF repeater
                  Enable Interrupts
               End If
            Else                                            'if on TX
               If Set_band = 0 Then
                  Setfont Font10x16tt                       'display active freq (TX freq. is different than RX freq.)
                  Lcdat 1 , 1 , Ftx_r1 ; " "
                  Setfont Font6x8
                  Lcdat 3 , 1 , "CH " ; Channel_rv ; " " ; Opis_v ; "   "       'VHF TX repeater
                  Else
                  Setfont Font10x16tt
                  Lcdat 1 , 1 , Ftx_r1_u ; " "
                  Setfont Font6x8
                  Lcdat 3 , 1 , "CH " ; Channel_ru ; " " ; Opis_u ; "   "       'UHF TX repeeater
               End If
               If Set_band = 1 Then                         'display non active freq.
                  Setfont Font6x8
                  Lcdat 5 , 1 , Ftx_r1 ; " MHz"
                  Lcdat 6 , 1 , "CH " ; Channel_rv ; " " ; Opis_v ; "   "       'VHF TX repeater
                  Else
                  Setfont Font6x8
                  Lcdat 5 , 1 , Ftx_r1_u ; " MHz"
                  Lcdat 6 , 1 , "CH " ; Channel_ru ; " " ; Opis_u ; "   "       'UHF TX repeater
               End If
         End If
         If Print_once = 1 Then
            Print_once = 0
            If Set_band = 0 Then                            'chose band
               If Channel_rv = 55 Or Channel_rv = 51 Or Channel_rv = 61 Then
                  Tx_ctcss = "0004"
                  Else
                  Tx_ctcss = "0018"
               End If
            Else
               If Channel_ru = 736 Then
                  Tx_ctcss = "0004"                         'subtones for UHF
                  Else
                  Tx_ctcss = "0018"
               End If
            End If
            'Initlcd
            Gosub Nastavi_repetitor
         End If
         Print
         If Krog = 1 Then
            'Config Powermode = Powerdown                    'turns off TX LED
            sleep
         End If
      '   Print
         Krog = Not Krog

      Loop Until Izhod = 1                                  'long encoder press
      Cls
      Izhod = 0
      C1 = 3
      Krog = 0
   End If



   If C1 = 3 Then
      Setfont Font6x8
      Lcdat 1 , 10 , " ECHOLINK " , 1
      Frtx_e1 = Frtx_e / 10000
      Setfont Font10x16tt
      Lcdat 3 , 1 , Frtx_e1                                 '; " MHz"
      Setfont Font6x8
      Disable Interrupts
      Lcdat 6 , 1 , " CH " ; Channel_echo ; " "             'echolink
      Enable Interrupts
      Select Case Channel_echo
         Case 1 : Frtx_e = 1445500
         Goto Semkaj
         Case 2 : Frtx_e = 1445750
         Goto Semkaj
         Case 3 : Frtx_e = 1446125
         Goto Semkaj
         Case 4 : Frtx_e = 1452250
         Goto Semkaj
         Case 5 : Frtx_e = 1452500
         Goto Semkaj
         Case 6 : Frtx_e = 1454750
         Goto Semkaj
   Semkaj:
         Tx_ctcss = "0018"
         Frtx_e1 = Frtx_e / 10000
      End Select

      If Print_once = 1 Then
         Print_once = 0
         Print "AT+DMOSETGROUP=" ; 0 ; "," ; Frtx_e1 ; "," ; Frtx_e1 ; "," ; Tx_ctcss ; "," ; Squ ; "," ; Rx_ctcss       '; "," ; 0       ' Echolink
      End If

   End If

   If C1 = 4 Then
      Cls
      Setfont Font6x8
      Lcdat 1 , 10 , " Free Freq. " , 1

      Do
         Gosub Ledice_sub

         Debounce Enk3 , 0 , Set_freq_sub , Sub             'encoder button to select menu
         Setfont Font10x16tt
         Lcdat 3 , 1 , Ftx1_free                            '; " MHz"
         Setfont Font6x8
         Select Case C2

            Case 1 :
               Step_freq = 125
               Lcdat 6 , 1 , "Step 12,5 kHz   "
            Case 2 :
               Step_freq = 1000
               Lcdat 6 , 1 , "Step 100 kHz   "
            Case 3 :
               Step_freq = 10000
               Lcdat 6 , 1 , "Step 1 MHz     "
            Case 4:
               Step_freq = 100000
               Lcdat 6 , 1 , "Step 10 MHz     "

            'Ftx_free = 1440000
         End Select

         If Print_once = 1 Then
            Print_once = 0
            Print "AT+DMOSETGROUP=" ; 0 ; "," ; Ftx1_free ; "," ; Ftx1_free ; "," ; Tx_ctcss ; "," ; Squ ; "," ; Rx_ctcss       ' ; "," ; 0       ' simplex
            Print "AT+DMOSETVOLUME=" ; Volume
            Print "AT+DMOSETMIC=" ; Mod_level ; "," ; 0 ; "," ; 0
         End If
         Gosub Ledice_sub

         Print
         If Krog = 1 Then
            'Config Powermode = Powerdown
            sleep
         End If
      '   Print
         Krog = Not Krog

      Loop Until Izhod = 1
      Cls
      Izhod = 0
      Step_freq = 125
      C1 = 5                                                '5 is because we jump to next menu
      Setupbyte = 1                                         '1 is because we want to display on LCD what we are aetting
   End If

   If C1 = 5 Then
      Cls
      Do
         Gosub Ledice_sub

         Lcdat 1 , 10 , " NASTAVITEV " , 1

         Debounce Enk3 , 0 , Setup_sub , Sub                'encoder button to select menu
         Select Case Setupbyte
            Case 1 :
               Lcdat 3 , 8 , "Squelch= " ; Squ ; "       "  'squelch
            Case 2 :
               Lcdat 3 , 8 , "Volume= " ; Volume ; "       "       'Volume
            Case 3 :
               Lcdat 3 , 8 , "Mod. level= " ; Mod_level ; "       "       'modulation level
         End Select
      Loop Until C1 = 1
      Cls
      C1 = 1
      Krog = 0                                              '
   End If

   Debounce Enk3 , 0 , Povecaj_c , Sub                      'encoder button

   Print
   If Krog = 1 Then

      Config Powermode = Powerdown
   End If
'   Print
   Krog = Not Krog
   'Print_once = Not Print_once

Loop

Nastavi_simpleks:
      If C1 = 1 Then
         Print_once = 0
         If Set_band = 0 Then
            Print "AT+DMOSETGROUP=" ; 0 ; "," ; Ftx1 ; "," ; Frx1 ; "," ; Tx_ctcss ; "," ; Squ ; "," ; Rx_ctcss       ' ; "," ; 0       ' VHF simplex
            Else
            Print "AT+DMOSETGROUP=" ; 0 ; "," ; Ftx1_u ; "," ; Frx1_u ; "," ; Tx_ctcss ; "," ; Squ ; "," ; Rx_ctcss       ' ; "," ; 0       ' UHF simplex
         End If
      End If
Return

Nastavi_repetitor:
      'If C1 = 2 Then
         If Set_band = 0 Then
            Print "AT+DMOSETGROUP=" ; 0 ; "," ; Ftx_r1 ; "," ; Frx_r1 ; "," ; Tx_ctcss ; "," ; Squ ; "," ; Rx_ctcss       ' ; "," ; Channel_rv       'VHF repeaters
            Else
            Print "AT+DMOSETGROUP=" ; 0 ; "," ; Ftx_r1_u ; "," ; Frx_r1_u ; "," ; Tx_ctcss ; "," ; Squ ; "," ; Rx_ctcss       ' ; "," ; 0       'UHF repeaters
         End If
Return

Enkoder_sub:
   Disable Interrupts
   B2 = Encoder(enk1 , Enk2 , Up , Down , 0)
   'Gosub Down
   Enable Interrupts
Return


Enkoder_sub1:
   Disable Interrupts
        If Enk2 = 1 And Enk1 = 0 Then
          Change = 1
          Gosub Down
        End If
        If Enk2 = 0 And Enk1 = 1 Then
          Change = 1
          Gosub Down
        End If
   Enable Interrupts
   Stevec = 0
Return

Enkoder_sub2:
   Disable Interrupts
        If Enk1 = 1 And Enk2 = 0 Then
          Change = 1
          Gosub Up
        End If
        If Enk1 = 0 And Enk2 = 1 Then
          Change = 1
          Gosub Up
        End If
   Enable Interrupts
   Stevec = 0
Return


Down:
   If Change = 1 Then
      Change = 0
      Pojdi_down = Not Pojdi_down
      If Pojdi = 0 Then
         Decr Setupbyte
         If Setupbyte = 0 And Setupbyte <= 255 Then Setupbyte = 4
      End If

      Select Case C1

         Case 1 :
            If Set_band = 0 Then                            'change channels only when band is active
               Ftx = Ftx - Step_freq                        'simplex VHF
               Frx = Frx - Step_freq
               Decr Channel_v
               If Channel_v <= 15 Then
                  Channel_v = 47
                  Ftx = 1455875
                  Frx = 1455875
               End If
               Frx1 = Frx / 10000
               Ftx1 = Ftx / 10000
            Else
               Frx_u = Frx_u - Step_freq                    'simplex UHF
               Ftx_u = Frx_u

               Decr Channel_su
               If Channel_su <= 271 Then
                  Channel_su = 286
                  Frx_u = 4335750
                  Ftx_u = 4335750
               End If
               Ftx1_u = Ftx_u / 10000
               Frx1_u = Frx_u / 10000
            End If
         Case 2 :
            If Set_band = 0 Then
               Ftx_r = Ftx_r - Step_freq                    'repeater VHF
               Frx_r = Frx_r - Step_freq
               Decr Channel_rv
               If Channel_rv <= 47 Then
                  Channel_rv = 63
                  Frx_r = 1457875
                  Ftx_r = 1451875
               End If
               Frx_r1 = Frx_r / 10000
               Ftx_r1 = Ftx_r / 10000
            Else
               Ftx_r_u = Ftx_r_u - Step_freq                'repeater UHF
               Frx_r_u = Frx_r_u - Step_freq
               Decr Channel_ru
               If Channel_ru <= 683 Then
                  Channel_ru = 750
                  Frx_r_u = 4393750
                  Ftx_r_u = 4317750
               End If
               Ftx_r1_u = Ftx_r_u / 10000
               Frx_r1_u = Frx_r_u / 10000
            End If
         Case 3 : Decr Channel_echo
            If Channel_echo <= 0 Or Channel_echo > 10 Then
               Channel_echo = 6
               Frtx_e = 1454750
            End If
            Frtx_e1 = Frtx_e / 10000

         Case 4 : Ftx_free = Ftx_free - Step_freq
            If Ftx_free < 1340000 Then Ftx_free = 1340000   'lower limit
            Ftx1_free = Ftx_free / 10000

      End Select

      If Pojdi = 1 Then
         Select Case Setupbyte
         Case 1 : Decr Squ
            If Squ <= 255 And Squ > 10 Then Squ = 0

         Case 2 : Decr Volume
            If Volume = 255 Then Volume = 0

         'Case 3 : Add_rx = Not Add_rx
         '   select/deselect additional 430 MHz reception

         End Select
      End If
   End If
   Print_once = 1                                           'every change of Encoder prints data to DRA818
Return

'--------------------------------------------------------------
Up:
   If Change = 1 Then
      Change = 0
      Pojdi_up = Not Pojdi_up
      If Pojdi = 0 Then
         Incr Setupbyte
         If Setupbyte => 5 And Setupbyte < 10 Then Setupbyte = 1
      End If

      Select Case C1
         Case 1 :
            If Set_band = 0 Then
               Ftx = Ftx + Step_freq                        'simplex VHF
               Frx = Frx + Step_freq
               Incr Channel_v
               If Channel_v >= 48 Then
                  Channel_v = 16
                  Ftx = 1452000
                  Frx = 1452000
               End If
               Frx1 = Frx / 10000
               Ftx1 = Ftx / 10000
            Else
               Frx_u = Frx_u + Step_freq                    'simplex UHF
               Ftx_u = Frx_u

               Incr Channel_su
               If Channel_su >= 288 Then
                  Channel_su = 272
                  Frx_u = 4334000
                  Ftx_u = 4334000
               End If
               Ftx1_u = Ftx_u / 10000
               Frx1_u = Frx_u / 10000
            End If
           ' If Channel_s = 24 Then
           '    Channel_s = 8
           ' End If

         Case 2 :
            If Set_band = 0 Then
               Ftx_r = Ftx_r + Step_freq                    'repetitor VHF
               Frx_r = Frx_r + Step_freq
               Incr Channel_rv
               If Channel_rv >= 64 Then
                  Channel_rv = 48
                  Frx_r = 1456000
                  Ftx_r = 1450000
               End If
               Frx_r1 = Frx_r / 10000
               Ftx_r1 = Ftx_r / 10000
            Else
               Ftx_r_u = Ftx_r_u + Step_freq                'repetitor UHF
               Frx_r_u = Frx_r_u + Step_freq
               Incr Channel_ru
               If Channel_ru >= 751 Then
                  Channel_ru = 684
                  Frx_r_u = 4385500
                  Ftx_r_u = 4309500
               End If
               Ftx_r1_u = Ftx_r_u / 10000
               Frx_r1_u = Frx_r_u / 10000
            End If

         Case 3 : Incr Channel_echo
            If Channel_echo >= 7 Then
               Channel_echo = 1
               Frtx_e = 1445500
            End If
            Frtx_e1 = Frtx_e / 10000

         Case 4 : Ftx_free = Ftx_free + Step_freq
            If Ftx_free > 4900000 Then Ftx_free = 4900000   'upper limit
            Ftx1_free = Ftx_free / 10000

      End Select

      If Pojdi = 1 Then
         Select Case Setupbyte
            Case 1 : Incr Squ
               If Squ >= 9 And Squ < 11 Then Squ = 8

            Case 2 : Incr Volume
               If Volume = 9 Then Volume = 8

            'Case 3 : Add_rx = Not Add_rx
         '   select/deselect additional 430 MHz reception


         End Select
      End If

   End If
   Print_once = 1                                           'every change of encoder prints data to DRA818
Return

'--------------------------------------------------------------

Setup_sub:
Pojdi = 1
Gosub Biper_s
   Do
          Select Case Setupbyte

            Case 1 :                                        'Lcd "Squelch =" ; Squ ; "             "
               Squ_e = Squ
               Lcdat 3 , 8 , "Squelch= " ; Squ ; "       "  'squelch
            Case 2 :                                        'Lcd "Volume =" ; Volume ; "     "
               Volume_e = Volume
               Lcdat 3 , 8 , "Volume= " ; Volume ; "       "       'Volume
            Case 3 :                                        'Lcd "Add. RX on 400 MHz" ; Add_rx ; "          "
               Lcdat 3 , 8 , "Mod. level= " ; Mod_level ; "       "       'modulation level
         End Select


   If Enk3 = 0 Then                                         'return
      Waitms 300
      If Enk3 = 0 Then
         Pojdi = 0
         Gosub Biper_s
         Gosub Biper_s
         Exit Do
      End If
   End If



   Loop Until Pojdi = 0
   Print "AT+DMOSETGROUP=" ; 0 ; "," ; Ftx1 ; "," ; Frx1 ; "," ; Tx_ctcss ; "," ; Squ ; "," ; Rx_ctcss       '; "," ; 0       ' simplex
   Waitms 100
   Print "AT+DMOSETVOLUME=" ; Volume
   'Waitms 100
   'Print "AT+DMOMES=" ; "7ABCDEFG"
   'Print "AT+DMOSETMIC=" ; Mod_level ; "," ; 0              '; "," ; 0    'mic level,scramble level
   Waitms 50
C1 = 1
Return
'********************************************
Povecaj_c:
   Gosub Biper_s
   Incr C1
   If C1 = 5 Then C1 = 1
   Print_once = 1
   'Gosub Biper_s
   Stevec = 0

Return
'********************************************

Biper_s:
   Bipbip = 0                                               'Beeper on Portd.0
   Waitms 50
   Bipbip = 1
   Waitms 50
Return
'********************************************
Set_freq_sub:
Print_once = 1
   Freq_scan = Not Freq_scan                                'press shortly encoder to enable scanning
   Gosub Biper_s
   Set_band = Not Set_band
   Incr C2
   If C2 >= 5 Then C2 = 1
   If Enk3 = 0 Then                                         'return
      Waitms 300
      If Enk3 = 0 Then
         Pojdi = 0
         Gosub Biper_s
         Gosub Biper_s
         Izhod = 1
      End If
   End If
   Stevec = 0
Return


Ledice_sub:
If Ptt = 0 Then
   Tx_bit = 1
   Tx_led = 1
   Rx_led = 0
   Else
   Rx_led = 1
   Tx_led = 0
End If
Return

'------------------------------------------------------------------
Tim0_isr:
Incr Clock1
   If Clock1 > 3906 Then                                    'za 12 MHz= 9375 za 200msek, za 8 MHz = 31249  , 121= 16 bitni timer je Timer1!!
      Clock1 = 0
      Incr Stevec
   End If
   If Stevec > 3 Then Stevec = 4
Return
'------------------------------------------------------------------
Start_timer_sub:
   Gosub Biper_s
   Stevec = 4
   'Start Timer0
Return

Okvir:
$bgf "okvir.bgf"

$include "font6x8.font"
$include "font10x16TT.font"'13x12_segoe.font "              '"Digital10x16.font"
'$include "Font11x14a.font"                                  ' "My12_16.font"