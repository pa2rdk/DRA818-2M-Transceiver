'DRA818V, copyright Jurij Mikeln, March 2015
'www.svet-el.si/english


$hwstack = 40
$swstack = 64
$framesize = 64

$regfile = "m168pdef.dat"
'$regfile = "m32def.dat"
$crystal = 1000000

$baud = 9600


Dim Bw As Bit                                               ' bandwith in KHz ( 0= 12.5KHz or 1= 25KHz )
Bw = 0                                                      '12.5 kHz channel_s

Dim Ftx As Single                                           '
Dim Ftx1 As Single                                          '

Ftx = 1452000                                               ' Tx frequency in MHz (134.0000 - 174.0000)
Dim Frx As Single
Dim Frx1 As Single
Frx = 1452000                                               ' Rx Frequency In MHz(134.0000 - 174.0000)

Dim Ftx_r As Single                                         '
Dim Ftx_r1 As Single                                        '
Dim Frx_r As Single
Dim Frx_r1 As Single
Ftx_r = 1450000                                             ' Tx Repeater Frequency In MHz(134.0000 - 174.0000)
Frx_r = 1456000                                             ' Rx Repeater frequency in MHz (134.0000 - 174.0000)

Dim Frtx_e As Single                                        'echolink freq
Dim Frtx_e1 As Single

Frtx_e = 1445500


Dim Ftx_free As Single                                      'free freq. set
Dim Ftx1_free As Single                                     '
Ftx_free = 1440000


Dim Tx_ctcss As String * 4
Tx_ctcss = "0000"                                           'Ctcss TX Frequency(0000 - 0038 ) ; 0000 = "no CTCSS"
Dim Rx_ctcss As String * 4
Rx_ctcss = "0000"                                           'Ctcss RX frequency ( 0000 - 0038 ); 0000 = "no CTCSS"
Dim Squ_e As Eram Byte
Dim Squ As Byte

Squ = Squ_e
                                                     'squelch level ( 0 - 8 ); 0 = "open"
Dim Channel_s As Byte                                       'simplex channel
Channel_s = 8
Dim Channel_v As Byte
Channel_v = 16                                              'V channels
Dim Channel_rs As Byte
Channel_rs = 0                                              'repeater old channel
Dim Channel_rv As Byte                                      'repeater new channels
Channel_rv = 48

Dim Channel_echo As Byte                                    'repeater ECHOlink channels
Channel_echo = 1

Dim Mod_level As Byte
Mod_level = 8

Dim Volume_e As Eram Byte
Dim Volume As Byte
Volume = Volume_e

Dim Tx_bit As Bit
Tx_bit = 0                                                  'receiving
                                              'Hz
Tx_led Alias Portb.5
Config Tx_led = Output

Rx_led Alias Portb.6
Config Rx_led = Output

'High_power_led Alias Portb.7
'Config High_power_led = Output

Dim Pojdi As Bit                                            'confirm with encoder switch

Bipbip Alias Portb.0                                        'beeper output
Config Bipbip = Output
Bipbip = 1                                                  'does not beep

Dim Y0 As Dword

Dim Freq_incr As Dword , Step_freq As Single
Step_freq = 125                                             '= 12.5 kHz

Freq_incr = 0

Dim C1 As Byte                                              'select menu
Dim C2 As Byte                                              'menu for step freq
Dim Setupbyte As Byte                                       'select setup menu
Setupbyte = 1

C2 = 1

Config Single = Scientific , Digits = 4

C1 = 1

Dim Pomoznibit As Bit                                       '
Pomoznibit = 0                                              '=0 counting up, =1 counting down

Dim Change As Bit

Dim Pojdi_up As Bit                                         '   pojdi=1 parameter set
Dim Pojdi_down As Bit

Dim Print_once As Bit                                       '=1 can print to UART, =0 cannot print
Print_once = 0


Dim Num As Byte , Num1 As Byte


Config Lcdbus = 4
Config Lcd = 16 * 2                                         '

Const Set_board = 1                                         'MegaPin board=0, VHF board =1

#if Set_board = 0                                           'MegaPin
Config Lcdpin = Pin , Db4 = Portc.4 , Db5 = Portc.5 , Db6 = Portc.6 , Db7 = Portc.7 , E = Portc.2 , Rs = Portc.1       'works in MegaPin
Set_power Alias Portd.4                                     '=0 >>low power, =1>> high power
Config Set_power = Output
Set_power = 0
#else
Config Lcdpin = Pin , Db4 = Portc.2 , Db5 = Portc.3 , Db6 = Portc.4 , Db7 = Portc.5 , E = Portc.0 , Rs = Portc.1       'works in VHF PCB
Set_power Alias Portb.3                                     '=0 >>low power, =1>> high power
Config Set_power = Output
Set_power = 0
#endif

Initlcd

Cursor Off
Upperline
Lcd "    2m TRX    "
Lowerline
Lcd "AX elektronika"
Gosub Biper_s
Wait 1

Enk1 Alias Pind.3                                           'set Encoder pins
Enk2 Alias Pind.2
Enk3 Alias Pind.4

Config Enk1 = Input
Config Enk2 = Input
Config Enk3 = Input
Portd.4 = 1
Portd.2 = 1
Portd.3 = 1

Dim Krog As Bit                                             '=0,first time around do-loop; =1 second time around do-loop
Krog = 0

Ptt Alias Pinb.4
Config Ptt = Input
Ptt = 1

Dim Ser_input As String * 15


Dim B2 As Byte
Enable Interrupts
Config Int0 = Falling
Config Int1 = Falling


Enable Int1
Enable Int0
Pojdi_up = 1                                                'Simplex channel is changed
Pojdi_down = 1                                              'Simplex channel is changed


On Int0 Enkoder_sub1
On Int1 Enkoder_sub2

Deflcdchar 0 , 12 , 18 , 18 , 12 , 4 , 5 , 6 , 5            ' OK sign
Deflcdchar 1 , 17 , 10 , 4 , 10 , 17 , 32 , 32 , 32         ' not OK sign

Cls

Frx1 = Frx / 10000
Ftx1 = Ftx / 10000
Frx_r1 = Frx_r / 10000
Ftx_r1 = Ftx_r / 10000
Ftx1_free = Ftx_free / 10000


If C1 = 1 Then Print "AT+DMOSETGROUP=" ; 0 ; "," ; Ftx1 ; "," ; Frx1 ; "," ; Tx_ctcss ; "," ; Squ ; "," ; Rx_ctcss       ' simplex
If C1 = 2 Then Print "AT+DMOSETGROUP=" ; 0 ; "," ; Ftx_r1 ; "," ; Frx_r1 ; "," ; Tx_ctcss ; "," ; Squ ; "," ; Rx_ctcss       ' repeaters
If C1 = 3 Then Print "AT+DMOSETGROUP=" ; 0 ; "," ; Frtx_e1 ; "," ; Frtx_e1 ; "," ; Tx_ctcss ; "," ; Squ ; "," ; Rx_ctcss       ' Echolink

'interrupt enable for encoder + encoder switch
Pcicr.2 = 1
Pcmsk2 = &B00011100

'interrupt enable for PTT switch
Pcicr.0 = 1
Pcmsk0.4 = 1                                                'on PCINT4

Print_once = 1                                              'at first poweron writes parameters in the module

Do


Pojdi = 0

Gosub Ledice_sub

If Ptt = 1 And Tx_bit = 1 Then                              ' to upload correct Rx freq after Tx is over
   Tx_bit = 0
   Waitms 500
   Print_once = 1
End If


  ' If Bw = 1 Then                 ', this IF sentence is not used, bandwidth is set to 12.5 kHz
  '    Step_freq = 250
  '    Else
  '    Step_freq = 125
  ' End If

   Num1 = Channel_v Mod 2
   Num = Channel_rv Mod 2

   Debounce Enk3 , 0 , Povecaj_c , Sub                      'encoder button to select menu

   Select Case Channel_v
      Case 16 : Channel_s = 8
      Case 17 : Channel_s = 8
      Case 18 : Channel_s = 9
      Case 19 : Channel_s = 9
      Case 20 : Channel_s = 10
      Case 21 : Channel_s = 10
      Case 22 : Channel_s = 11
      Case 23 : Channel_s = 11
      Case 24 : Channel_s = 12
      Case 25 : Channel_s = 12
      Case 26 : Channel_s = 13
      Case 27 : Channel_s = 13
      Case 28 : Channel_s = 14
      Case 29 : Channel_s = 14
      Case 30 : Channel_s = 15
      Case 31 : Channel_s = 15
      Case 32 : Channel_s = 16
      Case 33 : Channel_s = 16
      Case 34 : Channel_s = 17
      Case 35 : Channel_s = 17
      Case 36 : Channel_s = 18
      Case 37 : Channel_s = 18
      Case 38 : Channel_s = 19
      Case 39 : Channel_s = 19
      Case 40 : Channel_s = 20
      Case 41 : Channel_s = 20
      Case 42 : Channel_s = 21
      Case 43 : Channel_s = 21
      Case 44 : Channel_s = 22
      Case 45 : Channel_s = 22
      Case 46 : Channel_s = 23
      Case 47 : Channel_s = 23
    End Select

    Select Case Channel_rv
      Case 48 : Channel_rs = 0
      Tx_ctcss = "0018"
      Case 49 : Channel_rs = 0
      Tx_ctcss = "0018"
      Case 50 : Channel_rs = 1
      Tx_ctcss = "0018"
      Case 51 : Channel_rs = 1
      Tx_ctcss = "0004"
      Case 52 : Channel_rs = 2
      Tx_ctcss = "0018"
      Case 53 : Channel_rs = 2
      Tx_ctcss = "0018"
      Case 54 : Channel_rs = 3
      Tx_ctcss = "0018"
      Case 55 : Channel_rs = 3
      Tx_ctcss = "0004"
      Case 56 : Channel_rs = 4
      Tx_ctcss = "0018"
      Case 57 : Channel_rs = 4
      Tx_ctcss = "0018"
      Case 58 : Channel_rs = 5
      Tx_ctcss = "0018"
      Case 59 : Channel_rs = 5
      Tx_ctcss = "0018"
      Case 60 : Channel_rs = 6
      Tx_ctcss = "0018"
      Case 61 : Channel_rs = 6
      Tx_ctcss = "0004"
      Case 62 : Channel_rs = 7
      Tx_ctcss = "0018"
      Case 63 : Channel_rs = 7
      Tx_ctcss = "0018"
    End Select



   Upperline
   If C1 = 1 Then Lcd "Freq=" ; Frx1 ; "MHz" ; "        "   'simpleks channels
   If Ptt = 1 Then
      If C1 = 2 Then Lcd "Freq=" ; Frx_r1 ; "MHz" ; "        "       'repeater channels RX
      Else
      If C1 = 2 Then Lcd "Freq=" ; Ftx_r1 ; "MHz" ; "        "       'repeater channels TX
   End If
   If C1 = 3 Then Lcd "Freq=" ; Frtx_e1 ; "MHz" ; "        "       'Echolink channels

   If C1 = 4 Then Lcd "Freq=" ; Ftx1_free ; "MHz" ; "        "       'free freq. setting

   Lowerline
   If C1 = 1 Then
      Tx_ctcss = "0000"
      If Num1 = 0 Then
         Lcd "CH.S" ; Channel_s ; " CH.V" ; Channel_v ; "    "
      Else
         Lcd "CH.S" ; Channel_s ; "x CH.V" ; Channel_v ; "    "
      End If
   End If

   If C1 = 2 Then
      If Num = 0 Then
         Lcd "CH.R" ; Channel_rs ; " CH.RV" ; Channel_rv ; "    "
      Else
         Lcd "CH.R" ; Channel_rs ; "x CH.RV" ; Channel_rv ; "    "
      End If
   End If

   If C1 = 3 Then
      Lcd "CH.E" ; Channel_echo ; "             "
      Tx_ctcss = "0018"
   End If

   If C1 = 4 Then
      Cls
      Do
         Debounce Enk3 , 0 , Set_freq_sub , Sub             'encoder button to select menu
         Locate 1 , 1

         Select Case C2

            Case 1 : Lcd "   Step 12.5 kHz              "   '
               Step_freq = 125

            Case 2 : Lcd "   Step 100 kHz              "    '
               Step_freq = 1000

            Case 3 : Lcd "   Step 1MHz                 "    '
               Step_freq = 10000

         End Select

         Locate 2 , 1
         Lcd "Freq=" ; Ftx1_free ; "MHz" ; "          "
         If Print_once = 1 Then
            Print_once = 0
            Print "AT+DMOSETGROUP=" ; 0 ; "," ; Ftx1_free ; "," ; Ftx1_free ; "," ; Tx_ctcss ; "," ; Squ ; "," ; Rx_ctcss       ' simplex
            Print "AT+DMOSETVOLUME=" ; Volume
            Print "AT+DMOSETMIC=" ; Mod_level ; "," ; 0
         End If
         Gosub Ledice_sub                                   'it's got to jump to this sub or else RX LED doea not glow while TX
      Loop Until C1 = 1
      Step_freq = 125
      C1 = 5                                                '5 set value when we exit menu
      Setupbyte = 1                                         '1 it shows on LCD what we set
   End If


   If C1 = 5 Then
      Cls
      Do

         Debounce Enk3 , 0 , Setup_sub , Sub                'encoder button to select menu

         Locate 1 , 1
         Select Case Setupbyte

            Case 1 : Lcd "Squelch =" ; Squ ; "             "
               Squ_e = Squ

            Case 2 : Lcd "Volume =" ; Volume ; "     "
               Volume_e = Volume

            Case 3 : Lcd "Mod. level =" ; Mod_level ; "          "
            'Case 4 : Lcd "Set freq. manualy" ; Frx ; "           "

         End Select

         Locate 2 , 1
         Lcd "Setup Menu" ; "          "
         'Squ_e = Squ

      Loop Until C1 = 1

   End If


   If Print_once = 1 Then
      Print_once = 0

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
         'Tx_ctcss = "0018"
         Frtx_e1 = Frtx_e / 10000
      End Select

      If C1 = 1 Then
         Print "AT+DMOSETGROUP=" ; 0 ; "," ; Ftx1 ; "," ; Frx1 ; "," ; Tx_ctcss ; "," ; Squ ; "," ; Rx_ctcss       ' simplex
         Ser_input = Inkey()
         If Ser_input = "+DMOSETGROUP:0" Then
            Locate 2 , 15                                   'purpose is to show if module was correctly set, if yes, then show OK sign
            Lcd Chr(0)
            Else                                            'else
            Lcd Chr(1)                                      'show "x" sign
         End If                                             'but in this version this is disabled
      End If                                                'because I write blank spaces in the lower lines



      If C1 = 2 Then
         Print "AT+DMOSETGROUP=" ; 0 ; "," ; Ftx_r1 ; "," ; Frx_r1 ; "," ; Tx_ctcss ; "," ; Squ ; "," ; Rx_ctcss       ' repeaters
         Ser_input = Inkey()
         If Ser_input = "+DMOSETGROUP:0" Then
            Locate 2 , 15
            Lcd Chr(0)
            Else
            Lcd Chr(1)
         End If
      End If


      If C1 = 3 Then
         Print "AT+DMOSETGROUP=" ; 0 ; "," ; Frtx_e1 ; "," ; Frtx_e1 ; "," ; Tx_ctcss ; "," ; Squ ; "," ; Rx_ctcss       ' Echolink
         Ser_input = Inkey()
         If Ser_input = "+DMOSETGROUP:0" Then
            Locate 2 , 15
            Lcd Chr(0)
            Else
            Lcd Chr(1)
         End If
      End If


      If C1 = 4 Then
         Print "AT+DMOSETGROUP=" ; 0 ; "," ; Ftx1_free ; "," ; Ftx1_free ; "," ; Tx_ctcss ; "," ; Squ ; "," ; Rx_ctcss       ' free freq set
         Ser_input = Inkey()
         If Ser_input = "+DMOSETGROUP:0" Then
            Locate 2 , 15
            Lcd Chr(0)
            Else
            Lcd Chr(1)
         End If
      End If

      If C1 = 5 Then
         Print "AT+DMOSETVOLUME=" ; Volume
         Ser_input = Inkey()
         If Ser_input = "+DMOSETVOLUME:0" Then
            Locate 2 , 15
            Lcd Chr(0)
            Else
            Lcd Chr(1)
         End If

         'Waitms 100
         Print "AT+DMOSETMIC=" ; Mod_level ; "," ; 0
         Ser_input = Inkey()
         If Ser_input = "+DMOSETMIC:0" Then
            Locate 2 , 15
            Lcd Chr(0)
            Else
            Lcd Chr(1)
         End If

      End If

   End If
   Print
   Debounce Enk3 , 0 , Povecaj_c , Sub                      'encoder button to select menu



   If Krog = 1 Then Config Powermode = Powerdown            'Turn off TX LED or else it stays ON during powerdown
   Krog = Not Krog

Loop

Enkoder_sub:
   Disable Interrupts
   B2 = Encoder(enk1 , Enk2 , Up , Down , 0)

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

Return


Down:
   If Change = 1 Then
      Change = 0
      Pojdi_down = Not Pojdi_down
      If Pojdi = 0 Then
         Decr Setupbyte
         If Setupbyte = 0 And Setupbyte <= 255 Then Setupbyte = 3       'max value of submenu
      End If

      Select Case C1

         Case 1 : Ftx = Ftx - Step_freq
            Frx = Frx - Step_freq
            Decr Channel_v
            If Channel_v = 15 Then
               Channel_v = 47
               Ftx = 1455875
               Frx = 1455875
            End If
            Frx1 = Frx / 10000
            Ftx1 = Ftx / 10000

            If Channel_s = 7 Then
               Channel_s = 23
            End If

         Case 2 : Ftx_r = Ftx_r - Step_freq
            Frx_r = Frx_r - Step_freq
            Decr Channel_rv
            If Channel_rv = 47 Then
               Channel_rv = 63
               Frx_r = 1457875
               Ftx_r = 1451875
            End If
            Frx_r1 = Frx_r / 10000
            Ftx_r1 = Ftx_r / 10000

            If Channel_rs = 255 Then
               Channel_rs = 7
            End If

         Case 3 : Decr Channel_echo
            If Channel_echo = 0 Then
               Channel_echo = 6
               Frtx_e = 1454750
            End If
            Frtx_e1 = Frtx_e / 10000

         Case 4 : Ftx_free = Ftx_free - Step_freq
            Ftx1_free = Ftx_free / 10000

      End Select

      If Pojdi = 1 Then
         Select Case Setupbyte
         Case 1 : Decr Squ
            If Squ <= 255 And Squ > 10 Then Squ = 0

         Case 2 : Decr Volume
            If Volume = 255 Then Volume = 0

         Case 3 : Decr Mod_level
            If Mod_level <= 255 And Mod_level > 10 Then Mod_level = 0

         End Select
      End If
   End If
   Print_once = 1
Return

'--------------------------------------------------------------
Up:
   If Change = 1 Then
      Change = 0
      Pojdi_up = Not Pojdi_up
      If Pojdi = 0 Then                                     'prevents change of setupbyte in main menu
         Incr Setupbyte
         If Setupbyte => 5 And Setupbyte < 10 Then Setupbyte = 1       'min value of submeni
      End If

      Select Case C1
         Case 1 : Ftx = Ftx + Step_freq                     'incr freq
            Frx = Frx + Step_freq
            Incr Channel_v                                  'incr new channel
            If Channel_v = 48 Then                          'upper limit
               Channel_v = 16                               'lower limit
               Ftx = 1452000
               Frx = 1452000
            End If
            Frx1 = Frx / 10000                              'div to get right display and right freq is sent to module
            Ftx1 = Ftx / 10000

            If Channel_s = 24 Then
               Channel_s = 8
            End If

         Case 2 : Ftx_r = Ftx_r + Step_freq
            Frx_r = Frx_r + Step_freq
            Incr Channel_rv
            If Channel_rv = 64 Then
               Channel_rv = 48
               Frx_r = 1456000                              'receive freq
               Ftx_r = 1450000                              'transmit freq.
            End If
            Frx_r1 = Frx_r / 10000
            Ftx_r1 = Ftx_r / 10000


            If Channel_rs = 8 Then
               Channel_rs = 0
            End If

         Case 3 : Incr Channel_echo
            If Channel_echo = 7 Then
               Channel_echo = 1
               Frtx_e = 1445500
            End If
            Frtx_e1 = Frtx_e / 10000

         Case 4 : Ftx_free = Ftx_free + Step_freq
            Ftx1_free = Ftx_free / 10000

      End Select

      If Pojdi = 1 Then
         Select Case Setupbyte
         Case 1 : Incr Squ                                  'Squelch menu
               If Squ >= 9 And Squ < 11 Then Squ = 8

            Case 2 : Incr Volume                            'Volume menu - if needed
               If Volume = 9 Then Volume = 8
            Case 3 : Incr Mod_level                         'Modulation level menu
               If Mod_level >= 9 And Volume < 11 Then Mod_level = 8

         End Select
      End If

   End If
   Print_once = 1
Return

'--------------------------------------------------------------

Setup_sub:
Pojdi = 1
Gosub Biper_s
   Do
         Locate 1 , 1
         Select Case Setupbyte

            Case 1 : Lcd "Squelch =" ; Squ ; "             "
               Squ_e = Squ

            Case 2 : Lcd "Volume =" ; Volume ; "     "
               Volume_e = Volume

            Case 3 : Lcd "Mod. level =" ; Mod_level ; "          "

         End Select

   If Enk3 = 0 Then                                         'encoder switch pressed for long time to exit the do-loop
      Waitms 300
      If Enk3 = 0 Then
         Pojdi = 0
         Gosub Biper_s
         Gosub Biper_s
         Exit Do
      End If
   End If

   Loop Until Pojdi = 0
   Print "AT+DMOSETGROUP=" ; 0 ; "," ; Ftx1 ; "," ; Frx1 ; "," ; Tx_ctcss ; "," ; Squ ; "," ; Rx_ctcss       ' simplex
   Print "AT+DMOSETVOLUME=" ; Volume
   Print "AT+DMOSETMIC=" ; Mod_level ; "," ; 0
C1 = 1
Return
'--------------------------------------------------------------
Povecaj_c:                                                  'increment C1
   Incr C1                                                  'vhen encoder button is pressed
   If C1 = 6 Then C1 = 1
   Print_once = 1
   Gosub Biper_s
Return
'--------------------------------------------------------------

Biper_s:                                                    'beeper routine
   Bipbip = 0                                               'beep
   Waitms 50
   Bipbip = 1
   Waitms 50
Return
'--------------------------------------------------------------
Set_freq_sub:                                               'select step freq. in free freq. selection
   Gosub Biper_s
   Incr C2
   If C2 >= 4 Then C2 = 1
   If Enk3 = 0 Then                                         'encoder switch pressed for long time to exit the do-loop
      Waitms 300
      If Enk3 = 0 Then
         Pojdi = 0
         Gosub Biper_s
         Gosub Biper_s
         C1 = 1
      End If
   End If

Return

'--------------------------------------------------------------
Ledice_sub:                                                 'set proper LED to glow
   If Ptt = 0 Then                                          'when Rx or TX
      Tx_bit = 1
      Tx_led = 1
      Rx_led = 0
      Else
      Rx_led = 1
      Tx_led = 0
   End If
Return