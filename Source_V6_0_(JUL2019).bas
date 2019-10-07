'*****************************************************************
'*  Name    : Source.BAS                               *
'*  Author  : Jon Walker                                         *
'*  Notice  : Copyright (c) 2015                                 *
'*          :                                                    *
'*  Date    : 09/02/2018                                         *
'*  Version : 6.0                                               *
'*  Notes   :                                                    *
'*  MAY 2018: Initial Release  
'*  WORKS WELL ON REV2 PCB.  ATT WORKING
'*  OCT 2018 - Supports GUI                                     *
'*  Add Continuous sweep function
'*****************************************************************
Device = 18F26K22


Xtal = 64                         ' using pll at full speed (62.5nS/instruction)
Declare Bootloader = 0
Declare CCP1_Pin PORTC.2
Declare Hserial_Baud = 115200				' Set baud rate to 9600
Declare Hserial_RCSTA = %10010000       ' Enable serial port and continuous receive
Declare Hserial_TXSTA = %00100100       ' Enable transmit and asynchronous mode
Declare Hserial_Clear = On				' Enable Error clearing on received characters      ' Clear the buffer before receiving

;Declare Adin_Res = 10 ' 10-bit result required
;Declare Adin_Tad = 64_FOSC ' RC oscillator chosen
;Declare Adin_Stime = 50 ' Allow 50us sample time

;Watchdog = 0		; Disable the Watchdog timer      
 All_Digital = true
Config_Start
 ;FOSC = INTIO67         ' Int OSC Block
 FOSC = HSHP           ' HS oscillator (high power > 16 MHz)
 PLLCFG = On           ' Oscillator multiplied by 4
 PRICLKEN = On         ' Primary clock enabled
 FCMEN = Off           ' Fail-Safe Clock Monitor disabled
 IESO = Off            ' Internal/External Oscillator Switchover mode disabled
 PWRTEN = On           ' Power up timer enabled
 BOREN = SBORDIS       ' Brown-out Reset enabled in hardware only (SBOREN is disabled)
 BORV = 190            ' Brown Out Reset Voltage set to 1.90 V nominal
 WDTEN = Off           ' Watch dog timer is always disabled. SWDTEN has no effect.
 WDTPS = 128           ' Watchdog Timer Postscale 1:128
 CCP2MX = PORTC1       ' CCP2 input/output is multiplexed with RC1
 PBADEN = Off          ' PORTB<5:0> pins are configured as digital I/O on Reset
 CCP3MX = PORTB5       ' P3A/CCP3 input/output is multiplexed with RB5
 HFOFST = On           ' HFINTOSC output and ready status are not delayed by the oscillator stable status
 T3CMX = PORTC0        ' Timer3 Clock Input (T3CKI) is on RC0
 P2BMX = PORTB5        ' ECCP2 B (P2B) is on RB5
 MCLRE = EXTMCLR       ' MCLR pin enabled, RE3 input pin disabled
 STVREN = Off          ' Stack full/underflow will not cause Reset
 LVP = Off             ' Single-Supply ICSP disabled
 XINST = Off           ' Instruction set extension and Indexed Addressing mode disabled (Legacy mode)
 Debug = Off           ' Disabled
 Cp0 = off             ' Block 0 (000800-001FFFh) not code-protected
 CP1 = off             ' Block 1 (002000-003FFFh) not code-protected
 CP2 = off             ' Block 2 (004000-005FFFh) not code-protected
 CP3 = off             ' Block 3 (006000-007FFFh) not code-protected
 CPB = off             ' Boot block (000000-0007FFh) not code-protected
 CPD = Off             ' Data EEPROM not code-protected
 WRT0 = Off            ' Block 0 (000800-001FFFh) not write-protected
 WRT1 = Off            ' Block 1 (002000-003FFFh) not write-protected
 WRT2 = Off            ' Block 2 (004000-005FFFh) not write-protected
 WRT3 = Off            ' Block 3 (006000-007FFFh) not write-protected
 WRTC = Off            ' Configuration registers (300000-3000FFh) not write-protected
 WRTB = Off            ' Boot Block (000000-0007FFh) not write-protected
 WRTD = Off            ' Data EEPROM not write-protected
 EBTR0 = Off           ' Block 0 (000800-001FFFh) not protected from table reads executed in other blocks
 EBTR1 = Off           ' Block 1 (002000-003FFFh) not protected from table reads executed in other blocks
 EBTR2 = Off           ' Block 2 (004000-005FFFh) not protected from table reads executed in other blocks
 EBTR3 = Off           ' Block 3 (006000-007FFFh) not protected from table reads executed in other blocks
 EBTRB = Off           ' Boot Block (000000-0007FFh) not protected from table reads executed in other blocks
Config_End

EData $4F,$80,$12,$91,$00,$10,$00,$0E,$01,$00,$00,$FF,$00
'0 - 
'11 = PA ON 
'12 = ATTENUATION

'Dim CE        As PORTA.0
'Dim CLK       As PORTA.1
'Dim DAT       As PORTA.2
'Dim LE        As PORTA.3
'Dim MUX       As PORTA.4 

Dim CE        As LATA.0
Dim CLK       As LATA.1
Dim DAT       As LATA.2
Dim LE        As LATA.3
Dim MUX       As PORTA.4 

Dim PB        As PORTC.1            ' Input 
Dim B_CLK     As PORTC.0            ' Input 
Dim A_CLK     As PORTC.2            ' Input 
Dim LOCK_LED  As LATC.3
Dim PA_SW     As LATC.4           ' PA Switch 1 = ON
Dim VCO_SW    As LATC.5           ' VCO Switch 1 = ON
Dim URT_TX    As PORTC.6
Dim URT_RX    As PORTC.7

Dim P4        As LATB.0         ' Digital attenuator
Dim P3        As LATB.1
Dim P2        As LATB.2
Dim P1        As LATB.3
Dim P0        As LATB.4

Dim Timer0 As TMR0L.Word          ' access 2 8 bit timers as one 16bit onev
Dim Timer1 As TMR1L.Word
Dim T0IF As INTCON.2
Dim T1IF As PIR1.0


;----------------------   CONSTANTS    --------------------------------------




'-------------------------------------------------------------------------------------
'       
'                          Variable Declarations
'  
'-------------------------------------------------------------------------------------

Dim Temp As Byte        'Totally temporary byte
Dim Temp1 As Byte
Dim TEMP16 As Word
Dim LOOP16 As Word
Dim Pcount As Byte

Dim Rx_Buff[100] As Byte
Dim Rx_Pntr As Byte
Dim Buffer_Top As Byte  ' Flags
Dim ser_delay As Byte               ; programmable serial delay

Dim Latch As Dword      ;32 Bit
Dim F_Latch As Dword
Dim R_Latch As Dword
Dim N_Latch As Dword
Dim Index As Byte       ; used to count bits
Dim I2C_Bit As Bit


Dim Laststate As Bit
Dim Nowstate As Bit
Dim Power_Counter As Byte
Dim Power_Counter_Save As Byte      ; temp store during sweep
Dim FREQUENCY As Word                       ; VARIABLE TO STORE FREQUENCY OF PLL
'Hex Convert Variables
Dim Hex_pntr        As Byte                 ' Pointer for the Hex Byte conversion
Dim bytes2convert   As Byte           ' passes to convert routine and fills txbuff
Dim HA              As Byte                 ' First byte to covert in ASCII                                               
Dim HB              As Byte                 ' Second byte to convert in ASCII
Dim HC              As Byte                 ' Returned true Hex Byte
Dim DTX_Buff[100]   As Byte             ' buffer to hold the diseqc bytes
Dim DTX_ByteCnt     As Byte    ; Number of bytes in packet

'Lookup
Dim OFFSET As Word      ' Offset to call up the N_Latch
Dim ERR As Byte
Dim SWEEPLO As Word      '  Sweep Function
Dim SWEEPHI As Word      '  Sweep Function
Dim SWEEP_DELAY As Word
Dim CONT_SWEEP As Bit   ' Bit used to continuously sweep (B to Break)

Dim ATT_SET As Byte     ' User request Attenuation (0 - 25dB) - Cal
Dim ATT_CAL As Byte     ' Will be used to extract the Calibration levelling.  Target is 6dB to give 25dB use range

Dim FP_BIT As Bit       ' Frequency or Power Bit 0 = FREQUENCY, 1 = POWER
Dim PA_ON As Bit       ' If 1 PA on, IF 0 PA off
Dim PA_ON_Save As Bit   ' save power status


'**********************************************************
'               CONSTANTS Declarations
'**********************************************************

Dim Table_Limit As 500  'Table Limit is number of entries for Frequency and power
'-------------------------------------------------------------------------------------
'       
'                          SET UP PIC
'  
'-------------------------------------------------------------------------------------

        ANSELA =     %00000000    ; All Digital
        Clear ANSELB
        Clear ANSELC
        Clear INTCON
        Clear INTCON2
        Clear INTCON3
        SSPCON1 =  %00000000
        T3CON =     %00000000
        TRISA =     %00010000     ; RA4 input for Lock Detect
        TRISB =     %00000000     ; All output
        TRISC =     %10000111     ; RX input
'        VREFCON0 =  %11100000     ; 2.048V reference
 '       VREFCON1 =  %11101000
        OSCCON =    %01111000     ;16MHz EXT XTAL x 4
 	    OSCCON2 =   %10000100     ;Sys clock from 4xPLL       
;        T0CON =     %10000011     ; (64Mhz clk)16bit tmr:16 bit prescaler  Timeout 65.5ms.Timer on   (for diseqc RX timeout)
        T0CON =     %10000101     ; (64Mhz clk)16bit tmr:64 bit prescaler  Timeout 262.1ms.Timer on   (for diseqc RX timeout)
        T1CON =     %00110001     ; 16bit tmr:8 bit prescaler   ticks every 500nS    
        
;        ADCON0 =    %00000001     ; ADC on
 ;       ADCON1 =    %00001000     ; Vfref + to FVR(2.048V) Vref- to GND
  ;      ADCON2 =    %11110110     ; Clk 8TAD & Fosc/64        
        
GoTo    start        
'-------------------------------------------------------------------------------------
'       
'                          SUBROUTINES
'  
'-------------------------------------------------------------------------------------
 '*************************************************************************************
' 
' Name:                                 "Help"
' Passes:    None
' Returns:   None
' Description           Sends out a help file of commands
'*************************************************************************************

Help:
       Call Star_line
       HRSOut "PLL Source 2018",13
       HRSOut "Jon Walker(c) Email: Jon@MWAVE-LTD.com",13
       HRSOut "Rev 6.0 JUL 2019",13  
       HRSOut "Supports MWAVE SOURCE GUI",13        
       Call Star_line
       HRSOut "0 = 13.00 GHz",13
       HRSOut "1 = 11.20 GHz",13
       HRSOut "2 = 10.00 GHz",13       
       HRSOut "3 = 10.50 GHz",13
       HRSOut "4 = 10.60 GHz",13  
       HRSOut "5 = 11.00 GHz",13   
       HRSOut "6 = 10.50 GHz",13
       HRSOut "7 = 11.70 GHz",13
       HRSOut "8 = 12.20 GHz",13            
       HRSOut "9 = 12.70 GHz",13 
   '    HRSOut "A = Attenuator Setting 0 to 25dB (CMD = A00 to A25)",13       
       HRSOut "D = Decrease Frequency by 10MHz",13
       HRSOut "I = Increase Frequency by 10MHz",13       
       HRSOut "F = F12500 (TUNE TO 12500MHz).  Range 10.00 - 15.00GHz Step 10MHz",13 
       HRSOut "f = f12500 (TUNE TO 12500MHz).  Range 10.00 - 15.00GHz Step 10MHz",13                 
       HRSOut "L = Toggle Lock Led",13  
       HRSOut "M or m Stores current PLL Latches in EEPROM",13
       HRSOut "P = Increase power by 1dB",13  
       HRSOut "O = Decrease power by 1dB",13   
       HRSOut "Q = RF Power reset",13
       HRSOut "K = Sweep Attenuator min to max",13           
       HRSOut "R or r Resets PLL and recalls memory",13 
       HRSOut "S Sweep ONCE 01234 to 56789 Delay ABCD mS EX S1070012700D0500 10.70 - 12.70GHz Delay 500mS Step 10MHz",13 
       HRSOut "S Sweep CONTINUOUSLY ('B' to Break) 01234 to 56789 Delay ABCD mS EX S1070012700D0500Y 10.70 - 12.70GHz Delay 500mS Step 10MHz",13         
       HRSOut "s Sweep 10.70 to 12.75 delay 1000mS",13            
       HRSOut "T = Send 9 bytes to PLL (F_Latch:R_Latch:N_Latch) Example for 10GHz: T4F8012910010000C21",13  
       HRSOut "X = Power Off VCO, PA and Attenuator",13
       HRSOut "Y = Power On VCO, PA and Attenuator",13
       Call Star_line   
       Return
       
       
       
Star_line:
       For Temp = 1 To 40
            HRSOut "*"
       Next
       HRSOut 13
       Return
       


'*************************************************************************************
' 
' Name:                                 "i2c"
' Passes:    None
' Returns:   None
' Description           Sends out a help file of commands
'************************************************************************************* 
I2C_OUT:
        LE = 0      ;load enable low
        CLK = 0
        DelayUS 2
        Index = 24      '24 bits
        While Index > 0
            DAT = GetBit Latch,Index-1    'get bit
            DelayUS 5
            CLK = 1
            DelayUS 5
            CLK = 0 
            DelayUS 5
            Dec Index 
        Wend
        LE = 1                          'Load register
        DelayUS 5
        LE = 0
        Return
'*************************************************************************************
' 
' Name:                                 "SEND_ADF"
' Passes:    Latch
' Returns:   None
' Description           Sends out code to ADF PLL IC
'************************************************************************************* 
Send_ADF:
        CE = 0
        DelayUS 5
        Latch = F_Latch             ' Function Latch
        Call I2C_OUT
        Latch = R_Latch             ' Function Latch
        Call I2C_OUT
        Latch = N_Latch             ' Function Latch
        Call I2C_OUT
        CE = 1 
        Call UPDATE_GUI        
 ;       DelayMS 5
        Return  
'*************************************************************************************
' 
' Name:                                 "Incremental"
' Passes:    None
' Returns:   None
' Description           Remains in this loop scanning pushbutton
'*************************************************************************************         
Incremental:
        Clear Temp
INC_Loop:
        If Temp = 250 Then
            'HRSOut "EXIT PB",13
            HRSOut "{UI|SET|INFO.Text=EXIT PB}"             
            Return 
        EndIf
        Nowstate = A_CLK    ' read current state   
        
'Pushbutton Check            
        If PB = 0 Then 
            DelayMS 100
            While PB = 0:Wend
            FP_BIT = ~ FP_BIT    ' Toggle FP Bit 
            If FP_BIT = 0 Then
                'HRSOut "FREQUENCY SELECTED",13
                HRSOut "{UI|SET|INFO.Text=POWER SELECTED ON POT}"                 
            Else
                'HRSOut "POWER SELECTED",13
                HRSOut "{UI|SET|INFO.Text=FREQUENCY SELECTED ON POT}"                 
            EndIf           
        EndIf          
'Check if encoder moved        
        If Nowstate <> Laststate Then  
            Clear Temp                  ' Clear timer for no use                
            If B_CLK <> Nowstate Then
               ' HRSOut "RIGHTY",13
                    If FP_BIT = 1 Then
                        Call Power_Step_DN
                    Else
                        Call Freq_Step_UP
                    EndIf
            
            Else
               ' HRSOut "LEFTY",13
                    If FP_BIT = 1 Then
                        Call Power_Step_UP
                    Else
                        Call Freq_Step_DN
                    EndIf               
            EndIf 
            
            DelayMS 10                        
            Laststate = A_CLK           
         EndIf   
         DelayMS 10 
         Inc Temp
         GoTo INC_Loop                                
         Return
'************************************************************************************* 

'*************************************************************************************
' 
' Name:                                 "POWER STEP"
' Passes:    None
' Returns:   None
' Description:  Digital Attenuation Stepping
'*************************************************************************************   
         
Power_Step_UP:
        Inc Power_Counter
        If Power_Counter = 32 Then
            If PA_ON = 1 Then          'IF PA on then turn off <31
                PA_SW = 0
                PA_ON = 0
                Power_Counter = 0     ' Reset Att to min as PA OFF
                'HRSOut "PA OFF.  ATT = ",Dec2 Power_Counter, 13
                'HRSOut "{UI|SET|PA.Text=PA OFF}"
                HRSOut "{UI|SET|PA.Text=PA OFF}" 
                HRSOut "{UI|SET|ATT.Text=",Dec2 Power_Counter, "}"                                  
            Else 
                Power_Counter = 31
            EndIf
        EndIf
          GoTo Power_Set
Power_Step_DN:

        If Power_Counter = 0  Then
            If  PA_ON = 0 Then
                PA_SW = 1
                PA_ON = 1
                Power_Counter = 31     ' max att
                'HRSOut "PA ON.  ATT = ",Dec2 Power_Counter, 13
                HRSOut "{UI|SET|PA.Text=PA ON}" 
                HRSOut "{UI|SET|ATT.Text=",Dec2 Power_Counter, "}"                               
            EndIf              
        Else
            Dec Power_Counter
        EndIf    
        
Power_Set:
        Temp1 = ~Power_Counter 
        LATB = Temp1 @5
        'HRSOut "Power Setting = ", Bin8 Power_Counter," ", Dec2 Power_Counter,13
        HRSOut "{UI|SET|ATT.Text=",Dec2 Power_Counter, "}"         
        If PA_ON = 1 Then
            'HRSOut "PA ON",13
            HRSOut "{UI|SET|PA.Text=PA ON}"             
        Else
        'HRSOut "PA OFF",13 
            HRSOut "{UI|SET|PA.Text=PA OFF}"        
        EndIf       
        Return  
RF_OFF:
        Power_Counter_Save = Power_Counter  ' Save setting
        PA_ON_Save = PA_ON                  ' save PA counter
        PA_ON = 0                           ' flag PA off
        PA_SW = 0                           ' PA off
        'vco_sw = 0
        Power_Counter = 31                  ' max attenuation
        Temp1 = ~Power_Counter 
        LATB = Temp1 @5
        Return  

     
              
RF_ON:
        If PA_ON_Save = 1 Then              ' Restore PA settings
           PA_ON = 1
           PA_SW = 1
           'VCO_SW = 1           
        EndIf        
        Power_Counter = Power_Counter_Save
        Temp1 = ~Power_Counter 
        LATB = Temp1 @5
        Return
'*************************************************************************************
' 
' Name:                                 "FREQ STEP"
' Passes:    None
' Returns:   None
' Description:  
'*************************************************************************************               
Freq_Step_UP:
        'HRSOut "Frequeny Increment",13
        HRSOut "{UI|SET|INFO.Text=Frequeny Increment}"        
        If OFFSET = 501 Then Return     ' Limit to 15GHz
        Inc OFFSET
        Call Get_Latches
        Call Send_ADF
        Call Check_Lock          
        Return
        
Freq_Step_DN:
        'HRSOut "Frequeny Decrement",13
        HRSOut "{UI|SET|INFO.Text=Frequeny Decrement}"              
        If OFFSET = 0 Then GoTo NO_DEC
        Dec OFFSET
NO_DEC:        
        Call Get_Latches
        Call Send_ADF
        Call Check_Lock  
        Return         
        
'*************************************************************************************
' 
' Name:                                 "Check_Lock"
' Passes:    Latch
' Returns:   None
' Description           Checks PLL Lock and retries 3 times
'*************************************************************************************                    
Check_Lock:

        LOCK_LED = 0
        For Temp = 1 To 5
            Toggle LOCK_LED
            DelayMS 20
        Next
     
        If MUX = 1 Then 
            LOCK_LED = 1
            HRSOut 13,"PLL Lock OK!",13
            HRSOut "{UI|SET|LOCK.Text=LOCKED}"
            Return
        EndIf
        If MUX = 0 Then
            LOCK_LED = 0
           ' HRSOut "Failed to Lock, Retry 1",13
            HRSOut "{UI|SET|LOCK.Text=UNLOCKED}"            
            Call Send_ADF
        EndIf
        DelayMS 10
        If MUX = 1 Then 
            LOCK_LED = 1
          '  HRSOut 13,"PLL Lock OK!",13
            HRSOut "{UI|SET|LOCK.Text=LOCKED}"            
            Return
        EndIf
        If MUX = 0 Then
            LOCK_LED = 0
           ' HRSOut "Failed to Lock, Retry 2",13
            HRSOut "{UI|SET|LOCK.Text=UNLOCKED}"              
            Call Send_ADF            
        EndIf       
        If MUX = 1 Then 
            LOCK_LED = 1
            HRSOut 13,"PLL Lock OK!",13
            HRSOut "{UI|SET|LOCK.Text=LOCKED}"             
            Return
        EndIf
        If MUX = 0 Then
            LOCK_LED = 0
           ' HRSOut "Failed to Lock, Retry 3",13
            HRSOut "{UI|SET|LOCK.Text=UNLOCKED}"              
            Call Send_ADF            
        EndIf       
        If MUX = 1 Then 
            LOCK_LED = 1
            'HRSOut 13,"PLL Lock OK!",13
            HRSOut "{UI|SET|LOCK.Text=LOCKED}"             
            Return
        EndIf        
        If MUX = 0 Then
            LOCK_LED = 0
          '  HRSOut "Failed to Lock, Retry 4",13
            HRSOut "{UI|SET|LOCK.Text=UNLOCKED}"              
            Call Send_ADF            
        EndIf        
         If MUX = 1 Then 
            LOCK_LED = 1
            'HRSOut 13,"PLL Lock OK!",13
            HRSOut "{UI|SET|LOCK.Text=LOCKED}"            
            Return
        EndIf        
        If MUX = 0 Then
            LOCK_LED = 0
           ' HRSOut "Check Circuit or Code",13
            HRSOut "{UI|SET|INFO.Text=CHECK CIRCUIT}"                         
        EndIf       
        Return
        
LOCK_AT:
        'HRSOut "Attempting Lock at "  
            HRSOut "{UI|SET|INFO.Text=Attempting Lock At }"                          
        Return 
        
Fast_LOCK:        
        If MUX = 1 Then 
            LOCK_LED = 1
            Return
        EndIf
        If MUX = 0 Then
            LOCK_LED = 0
            Call Send_ADF
        EndIf  
        If MUX = 1 Then 
            LOCK_LED = 1
            Return
        EndIf

        LOCK_LED = 0    ; Failed to lock
                    
        Return
'*************************************************************************************
' 
' Name:                                 "GET_MEM"
' Passes:   
' Returns:   None
' Description           Gets Stored values from Eprom and loads PLL
'*************************************************************************************         
Get_Mem:
        'HRSOut "Recall Memory",13 
        HRSOut "{UI|SET|INFO.Text=RECALL MEMORY}"                  
        F_Latch.Byte2 = ERead 0
        F_Latch.Byte1 = ERead 1
        F_Latch.Byte0 = ERead 2
        R_Latch.Byte2 = ERead 3
        R_Latch.Byte1 = ERead 4
        R_Latch.Byte0 = ERead 5      
        N_Latch.Byte2 = ERead 6
        N_Latch.Byte1 = ERead 7
        N_Latch.Byte0 = ERead 8
        OFFSET.HighByte = ERead 9
        OFFSET.LowByte = ERead 10
        Temp = ERead 11
            If Temp = 0 Then
                PA_SW = 0
                PA_ON = 0
            Else
                PA_SW = 1
                PA_ON = 1
            EndIf
        Power_Counter = ERead 12    ' Store Counter
        Call Power_Set              ' Set Power                 
        Call Send_ADF               ' Program ADF Synth
        Call Check_Lock             ' Lock Check
        Return
'*************************************************************************************
' 
' Name:                                 "SAVE_MEM"
' Passes:   
' Returns:   None
' Description           SAVES DATA TO EEPROM
'*************************************************************************************          
SAVE_MEM:
                EWrite 0,[F_Latch.Byte2]
                EWrite 1,[F_Latch.Byte1]
                EWrite 2,[F_Latch.Byte0]
                EWrite 3,[R_Latch.Byte2]
                EWrite 4,[R_Latch.Byte1]
                EWrite 5,[R_Latch.Byte0]                
                EWrite 6,[N_Latch.Byte2]
                EWrite 7,[N_Latch.Byte1]
                EWrite 8,[N_Latch.Byte0]    
                EWrite 9,[OFFSET.Byte1]
                EWrite 10,[OFFSET.Byte0] 
                If PA_ON = 0 Then
                    EWrite 11,[$00]
                Else
                    EWrite 11,[$FF]
                EndIf
                EWrite 12,[Power_Counter] 
                'Call Star_line                         
                'HRSOut "STORED CURRENT LATCHES IN EEPROM",13
                'HRSOut "F_LATCH = $", Hex6 F_Latch,13
                'HRSOut "R_LATCH = $", Hex6 R_Latch,13
                'HRSOut "N_LATCH = $", Hex6 N_Latch,13 
                'HRSOut "OFFSET = ", Dec3 OFFSET,13
                'HRSOut "Power Counter = ", Dec2 Power_Counter, 13
                'If PA_ON = 1 Then
                    'HRSOut "PA ON",13
                'Else
                    'HRSOut "PA OFF",13
                'EndIf
                'Call Star_line
        HRSOut "{UI|SET|INFO.Text=SETTINGS SAVED}"                
        Return        
        
'*************************************************************************************
' 
' Name:                                 "HEXCONVERT"
' Passes:    
' Returns:   
' Description           Converts RXD ASCII to Decimal
'*************************************************************************************                         
HexConvert:
       DTX_ByteCnt = 0              ' number of bytes to be sent on bus
       Temp = Rx_Pntr - 1
     '  hrsout "Rx_Pntr =", Dec RX_Pntr,13
       Hex_pntr = 1
       Temp1 = 0                 
       While Hex_pntr < Temp    
            HA =  Rx_Buff[Hex_pntr]     ' On first pass the first byte is RX_Byte[1] as the "T" is in byte 0
            HA = HA - 48                   ' convert 0..9
            If HA > 9 Then HA = HA - 7      ' checks if numeric A....E           
            HC = HA * 16                    ' Place result in upper nibble of HC
            Inc Hex_pntr
            HA =  Rx_Buff[Hex_pntr]       ' Get the next byte    
            HA = HA - 48                   ' convert 0..9
            If HA > 9 Then HA = HA - 7      ' checks if numeric A....E
            Inc Hex_pntr
            DTX_Buff[Temp1] = (HC|HA)  ' Loaded into DTX_Buffer
            'hrsout 13, hex2 DTX_Buff[temp1],13
            Inc DTX_ByteCnt
            Inc Temp1
       Wend
      '  HRSOut "Byte_Cnt =", Dec DTX_ByteCnt,13  
      '   HRSOut "Hex_Pntr =", Dec hex_pntr,13        

    Return  
    
'************************************************************************************
' SUBROUTINE:       GET_OFFSET
' DESCRIPTION:      Converts ASCII receive buffer to decimal F12000 converts to 200
' RETURNS:          EITHER ERR=1 OR NUMBER IN OFFSET
'
'************************************************************************************    
GET_OFFSET:
                ' HRSOUT "$",HEX2 RX_BUFF[1], " BYTE 1",13
                '' HRSOut "$",Hex2 Rx_Buff[2], " BYTE 2",13
                 'HRSOut "$",Hex2 Rx_Buff[3], " BYTE 3",13                 
                 'HRSOut "$",Hex2 Rx_Buff[4], " BYTE 4",13                 
                                  

                ERR = 0
                OFFSET = 0                                  ' Clear offset
                If Rx_Buff[1] <> $31 Then                    ; must have 1
                    'HRSOut "Out of range",13 
                    HRSOut "{UI|SET|INFO.Text=ERROR OUT OF RANGE}"                    
                    ERR = 1
                    Return
                EndIf                    
                                                     
                If Rx_Buff[2] = $30 Then GoTo Dig3          ' if zero dont add anything
                OFFSET = (100 * (Rx_Buff[2]-$30))            ' get hundredds
Dig3:                                                       ' number is x000                
                If Rx_Buff[3] = $30 Then GoTo Dig2          ' if zero dont add anything
                OFFSET = OFFSET + (10 * ((Rx_Buff[3]-$30)))  ' get tens
Dig2:           
                If Rx_Buff[4] = $30 Then GoTo Dig1          ' if zero dont add anything
                OFFSET = OFFSET + (Rx_Buff[4]-$30)          ' get tens
Dig1:                       

                If OFFSET > Table_Limit Then 
                    'HRSOut "Out of Range",13
                    HRSOut "{UI|SET|INFO.Text=ERROR OUT OF RANGE}"                     
                    ERR = 1
                    Return
                EndIf
                'HRSOut "OFFSET = ",Dec3 OFFSET,13
                Return    
                
'************************************************************************************
' SUBROUTINE:       ATT
' DESCRIPTION:      SETS DIGITAL ATTENUATOR
' RETURNS:          EITHER ERR=1 OR NUMBER IN OFFSET
' P0 = RB4, P1 = RB3, P2 = RB2, P3 =  RB1, P4 = RB0
'************************************************************************************     
' 1 1 1 1 1 = 0
' 1 1 1 1 0 = 1   

 ATT:
            If OFFSET < Table_Limit Then
                ATT_CAL = LRead8 CAL_TABLE[OFFSET]      'get the Cal Setting from table if in range
            EndIf
            ERR = 0
            Temp = 0
            If ATT_SET > 25 Then
                ERR = 1
                'HRSOut "Only 25dB user Range",13
                Return
            EndIf
            ATT_SET = (ATT_SET + ATT_CAL)       ' IF ATT CAL = 5 and ATT_SET = 10 then ATT will be 15dB
            Temp = 0
            ATT_SET = ATT_SET @ 5       ' REVERSE ORDER OF LOWER 5 BITS (COMPLIMENT)  XXX00000 = XXX11111
            Temp.0 = ATT_SET.4          ' REVERSE THE PINS
            Temp.1 = ATT_SET.3
            Temp.2 = ATT_SET.2
            Temp.3 = ATT_SET.1
            Temp.4 = ATT_SET.0
            LATB = Temp                 ' PUT TO PORT
            'HRSOut "LATB = ", Bin8 Temp,13
            Return   
                
'************************************************************************************
' SUBROUTINE:       GET_Latches
' DESCRIPTION:      Selects the correct Function and reference latches depending on the frequency request
' RETURNS:          Returns F and R Latches 
'
'************************************************************************************                  
                
Get_Latches:
                TEMP16 = 10000  
                If OFFSET > 0 Then
                    TEMP16 = TEMP16 + (OFFSET * 10)                     
                EndIf   
;                HRSOut "Frequency = ",Dec6 TEMP16," MHz",13
                
                Temp = OFFSET ?0        ' Get the binary representation of the first digit (DIG in Manual)
                Select Temp
                    Case 0                            
                        F_Latch = $0F8012             ' Function Latch for 12.5MHz PFD    8/9
                        R_Latch = $910010             ' R Counter
                    Case 2
                        F_Latch = $4F8012             ' Function Latch for 5MHz PFD    16/17
                        R_Latch = $910028             ' R Counter                     
                    Case 4
                        F_Latch = $4F8012             ' Function Latch for 5MHz PFD    16/17
                        R_Latch = $910028             ' R Counter                                                
                    Case 5
                        F_Latch = $0F8012             ' Function Latch for 12.5MHz PFD    8/9
                        R_Latch = $910010             ' R Counter 
                    Case 6
                        F_Latch = $4F8012             ' Function Latch for 5MHz PFD    16/17
                        R_Latch = $910028             ' R Counter                      
                    Case 8
                        F_Latch = $4F8012             ' Function Latch for 5MHz PFD    16/17
                        R_Latch = $910028             ' R Counter                     
                                                               
                    Case Else                         ' 1,3,7 & 9
                        F_Latch = $4F8012             ' Function Latch for 2.5MHz PFD    16/17
                        R_Latch = $910050             ' R Counter                    
                EndSelect               
                N_Latch = LRead32 N_Table[OFFSET]                ' GET OFFSET from table
               ' HRSOut "F_LATCH = $", Hex6 F_Latch,13
               ' HRSOut "R_LATCH = $", Hex6 R_Latch,13
               ' HRSOut "N_LATCH = $", Hex6 N_Latch,13  
               
               If OFFSET = 485 Then
                        F_Latch = $4F8012             ' Function Latch for 12.5MHz PFD    8/9
                        R_Latch = $910010             ' R Counter 
                        N_Latch = $001225             ' > 14.8GHz need to put 16/17
                EndIf
               If OFFSET = 445 Then
                        F_Latch = $4F8012             ' Function Latch for 12.5MHz PFD    8/9
                        R_Latch = $910010             ' R Counter 
                        N_Latch = $001205             ' > 14.8GHz need to put 16/17
                EndIf                
                
                Return      
 
UPDATE_GUI:
                If PA_ON = 0 Then      
                    HRSOut "{UI|SET|PA.Text=PA OFF}"
                EndIf 
                If PA_ON = 1 Then
                     HRSOut "{UI|SET|PA.Text=PA ON}"
                EndIf 
                If MUX = 1 Then        
                    HRSOut "{UI|SET|LOCK.Text=PLL LOCKED}"
                EndIf 
                If MUX = 0 Then        
                    HRSOut "{UI|SET|LOCK.Text=PLL UNLOCKED}"
                EndIf                 
                HRSOut "{UI|SET|ATT.Text=",Dec2 Power_Counter, "}"
                FREQUENCY = 10000 + (OFFSET * 10)
                HRSOut "{UI|SET|FREQ.Text=",Dec5 FREQUENCY, " MHz}"    

                If  VCO_SW = 1  Then
                     HRSOut "{UI|SET|RF.Text=RF ON}"
                Else
                     HRSOut "{UI|SET|RF.Text=RF OFF}" 
                EndIf                                                                           
                Return 
 
    
    
'*************************************************************************************
'*************************************************************************************
' 
'                                   START
' 
' 
 '*************************************************************************************
'*************************************************************************************       

            

    
start:   
 
        Buffer_Top = 99         ' RS232 serial depth
        ser_delay = 50         ; initial 200mS serial delay before timeout
        LOCK_LED = 0   
        PA_SW = 1             ; PA on
        PA_ON = 1              ; Flag PA on for Att routine
        VCO_SW = 1              ; VCO on
        LATB = %00011111        ; no attenuation
        Power_Counter = 0
        DelayMS 1000
        Laststate = A_CLK    'read pin 
        OFFSET = 0              ' Offset is 0             
        ERR = 0
        Temp = 0
        TEMP16 = 0
        ATT_SET = 0
        ATT_CAL = 0  
        FP_BIT = 0    ' Select Frequency initially (Incremental Encoder)
        CONT_SWEEP = 0 ' dont sweep forever
                     
        Call Get_Mem                     
New_Msg:       
       Rx_Pntr = 0     ' top of stack


serial_buffer_fast:                  ' loop here once a byte RX'd so we dont miss one    
        HRSIn {ser_delay,Check}, Rx_Buff[Rx_Pntr] 
        Inc Rx_Pntr                         ' Dropped here as byte has arrived
        If Rx_Pntr = buffer_top+1 Then
            Rx_Pntr = 0
            'HRSOut "Stack Overflow Error!",13
            HRSOut "{UI|SET|INFO.Text=SERIAL BUFFER OVERFLOW}"             
        EndIf
        GoTo serial_buffer_fast 
Check:                           ; Every SER_delay we enter here if no data received           
        If Rx_Pntr = 0 Then       ; Receive buffer 
            Nowstate = A_CLK    ' read current state
            
            If Nowstate <> Laststate Then
                Call Incremental
                Laststate = Nowstate                
            EndIf  
                           
        If PB = 0 Then      
            Call SAVE_MEM
            While PB = 0:Wend
        EndIf  
                                           
            If MUX = 0 Then
                Toggle LOCK_LED 
                'Call UPDATE_GUI             
            EndIf
            If MUX = 1 Then 
                LOCK_LED = 1
                'Call UPDATE_GUI
            EndIf
            GoTo serial_buffer_fast
        EndIf
        
 '*******************  If we are here then there a number of bytes in the seiral port       
        
         LOCK_LED = 0
        
        'IF we are here as bytes are received 

        
        Select Rx_Buff[0]
            Case "0"                          
                Call LOCK_AT
'                HRSOut "13.00 GHz",13         ' 13.00 GHz
                HRSOut "{UI|SET|INFO.Text=MEMORY 0: 13.00GHz}"                 
                OFFSET = 300
                Call Get_Latches
                Call Send_ADF
                Call Check_Lock 
              '  Call ATT      
            Case "1"                          
                Call LOCK_AT
                'HRSOut "11.2 GHz",13          ' 11.2GHz
                HRSOut "{UI|SET|INFO.Text=MEMORY 1: 11.20GHz}"                  
                OFFSET = 120 
                Call Get_Latches
                Call Send_ADF
                Call Check_Lock
               ' Call ATT
            Case "2"                          
                Call LOCK_AT
                'HRSOut "10.00 GHz",13         ' 10.00 GHz  
                HRSOut "{UI|SET|INFO.Text=MEMORY 2: 10.00GHz}"                  
                OFFSET = 0          
                Call Get_Latches
                Call Send_ADF
                Call Check_Lock   
               ' Call ATT           
            Case "3"                          
                Call LOCK_AT
                OFFSET = 50
                'HRSOut "10.50 GHz",13         ' 10.50 GHz
                HRSOut "{UI|SET|INFO.Text=MEMORY 3: 10.50GHz}"                              
                Call Get_Latches
                Call Send_ADF
                Call Check_Lock
             '   Call ATT
            
            Case "4"                          
                Call LOCK_AT
                OFFSET = 60                
                'HRSOut "10.60 GHz",13         ' 10.60 GHz  
                HRSOut "{UI|SET|INFO.Text=MEMORY 4: 10.60GHz}"                  
                Call Get_Latches
                Call Send_ADF
                Call Check_Lock
               ' Call ATT
                                                
            Case "5"                          
                Call LOCK_AT
                'HRSOut "11.00 GHz",13         ' 11.00 GHz
                HRSOut "{UI|SET|INFO.Text=MEMORY 5: 11.00GHz}"                   
                OFFSET = 100            
                Call Get_Latches
                Call Send_ADF
                Call Check_Lock
             '   Call ATT
                               
            Case "6"                          
                Call LOCK_AT
'                HRSOut "11.25 GHz",13         ' 11.25 GHz
                HRSOut "{UI|SET|INFO.Text=MEMORY 6: 11.25GHz}"                        
                OFFSET = 125       
                Call Get_Latches
                Call Send_ADF
                Call Check_Lock
              '  Call ATT
            
            Case "7"                          
                Call LOCK_AT
                'HRSOut "11.70 GHz",13          ' 11.70 GHz
                HRSOut "{UI|SET|INFO.Text=MEMORY 7: 11.70GHz}"                   
                OFFSET = 170            
                Call Get_Latches
                Call Send_ADF
                Call Check_Lock
               ' Call ATT
                  
            Case "8"                          
                Call LOCK_AT
                'HRSOut "12.20 GHz",13         ' 12.20 GHz 
                HRSOut "{UI|SET|INFO.Text=MEMORY 8: 12.20GHz}"                        
                OFFSET = 220      
                Call Get_Latches
                Call Send_ADF
                Call Check_Lock
              '  Call ATT
                
            Case "9"                          
                Call LOCK_AT
                'HRSOut "12.70 GHz",13         ' 12.70 GHz 
                HRSOut "{UI|SET|INFO.Text=MEMORY 1: 12.70GHz}"                   
                OFFSET = 270           
                Call Get_Latches
                Call Send_ADF
                Call Check_Lock
                'Call ATT
                
            Case "A"
                Call HexConvert
                'ATT_SET = DTX_Buff[0]
               ' HRSOut "Attenuator Setting",Dec2 ATT_SET         'A00 to A31                
                'Call ATT              
                     
            Case "I"
                If OFFSET > Table_Limit Then OFFSET = Table_Limit -1
                'HRSOut "Increment Freq 10MHz,",13
                HRSOut "{UI|SET|INFO.Text=INCREMENT FREQ 10MHz}"                                  
                Inc OFFSET
                Call Get_Latches
                Call Send_ADF
                Call Check_Lock 
             '   Call ATT
            Case "D"   
                If OFFSET > 0 Then
                    Dec OFFSET
                EndIf
                'HRSOut "Decrement Freq 10MHz",13
                HRSOut "{UI|SET|INFO.Text=DECREMENT FREQ 10MHz}"                
                Call Get_Latches
                Call Send_ADF
                Call Check_Lock  
                'Call ATT                               
                                                     
            Case "h"
                Call Help
            Case "H"
                Call Help
            Case "f"    
                GoTo Here
            Case "F"   
Here:                                                 ' F12110 IS 12110 MHz but only 210 will be used
                If Rx_Pntr  <> 6 Then
                    'HRSOut "ERROR IN REQUEST.  EXAMPLE 'F12000' = 12000 MHz",13
                HRSOut "{UI|SET|INFO.Text=ERROR IN REQUEST.  EXAMPLE 'F12000' = 12000 MHz}"                    
                    GoTo out
                EndIf
                Call Star_line
                'HRSOut "Set Frequency from request",13
                Call GET_OFFSET                     ' Converts ASCII to decimal and places result in Offset
                If ERR = 1 Then GoTo out
                Call Get_Latches                    ' Gets all latches from tables
                Call Send_ADF                       ' Send Data
                Call Check_Lock                     ' Check for Lock
               ' Call ATT                            ' Set attenuation based on CAL and ATT request
        
            Case "G"
                Call UPDATE_GUI
                             
            Case "L"
                For Temp = 1 To 10
                    Toggle LOCK_LED
                    DelayMS 100
                Next
                LOCK_LED = 0
            Case "M"
                  Call SAVE_MEM
            Case "m"
                  Call SAVE_MEM  
            Case "P" 
                Call Power_Step_UP
            Case "O" 
                Call Power_Step_DN  
            Case    "p"                      ' Ping back the RX buffer
              '  HRSOut "p---->  PING",13
             '  HRSOut 13,Dec Rx_Pntr,13        
               While Rx_Pntr > 0      
                    HRSOut Rx_Buff[Rx_Pntr-1]
                    Dec Rx_Pntr
               Wend                 
            Case "Q"
                 Power_Counter = 0
                 PA_ON = 1
                 PA_SW = 1
                 Call Power_Set
                 Call UPDATE_GUI   
            Case "K"
                 PA_ON = 1
                 PA_SW = 1
                 For Pcount = 30 To 0   Step -1
                    Power_Counter =  Pcount
                    Call Power_Set
                    DelayMS 200
                    Call UPDATE_GUI
                 Next Pcount       
                
            Case "R"
                'HRSOut "RESET",13
                HRSOut "{UI|SET|INFO.Text=RESET}"                 
                GoTo start
            Case "r"
                'HRSOut "RESET",13
                HRSOut "{UI|SET|INFO.Text=RESET}"                 
                GoTo start 
            Case "S"                                    ' Command S 10700 12300  D  1000
               CONT_SWEEP = 0                           ' ASSUME NO LOOP OF SWEEP         
               If Rx_Pntr = 16 Then GoTo standard_sweep
               If Rx_Pntr = 17 Then
                   If  Rx_Buff[16] = "Y" Then CONT_SWEEP = 1 
               EndIf
standard_sweep:                  
               Call GET_OFFSET                              ' will return first byte in OFFSET
               SWEEPLO = OFFSET                         ' save sweep low figure
               'HRSOut "SWEEPLO = ", Dec3 SWEEPLO,13               
               Rx_Buff[1] = Rx_Buff[6]
               Rx_Buff[2] = Rx_Buff[7]
               Rx_Buff[3] = Rx_Buff[8]
               Rx_Buff[4] = Rx_Buff[9]  
               Call GET_OFFSET
               SWEEPHI = OFFSET
               'HRSOUT "SWEEPHI = ", DEC3 SWEEPHI,13
               OFFSET = SWEEPLO
               SWEEP_DELAY = 0
               If Rx_Buff[12] = $30 Then GoTo DDig3          ' if zero dont add anything
               SWEEP_DELAY = (1000 * (Rx_Buff[12]-$30))            ' get thousands
DDig3:                                                                      
               If Rx_Buff[13] = $30 Then GoTo DDig2          ' if zero dont add anything
               SWEEP_DELAY = SWEEP_DELAY + (100 * ((Rx_Buff[13]-$30)))  ' get hundereds
DDig2:           
               If Rx_Buff[14] = $30 Then GoTo DDig1          ' if zero dont add anything
               SWEEP_DELAY = SWEEP_DELAY + (10 * ((Rx_Buff[14]-$30)))          ' get tens
DDig1:     
               SWEEP_DELAY = SWEEP_DELAY + (Rx_Buff[15]-$30)          ' get units
               'HRSOut "SWEEP DELAY", Dec4 SWEEP_DELAY,13
               If SWEEP_DELAY = 0 Then SWEEP_DELAY = 1
            
;sweep code here
Go_again:
                Call RF_OFF
                For LOOP16 = SWEEPLO To SWEEPHI    Step 1              
                  OFFSET = LOOP16
                  Call Get_Latches                    ' Gets all latches from tables
                  Call Send_ADF                       ' Send Data
                  Call Fast_LOCK                      ' Check lock fast
                  Call RF_ON       
                  Temp = HRSIn,{SWEEP_DELAY,JMP_OVER}
                  If Temp = "B" Then
                    CONT_SWEEP = 0                  ' Cancel continuous sweep
                    GoTo out                         ' Exit routine
                  EndIf
JMP_OVER:                 
                Call RF_OFF
                Call UPDATE_GUI
                Next LOOP16                                                                              
                Call RF_ON      
                If CONT_SWEEP = 1 Then GoTo Go_again
                                             
            Case "s"                          
                For LOOP16 = 70 To 275                  ' 10700 TO 12750
                    OFFSET = LOOP16
                    Call Get_Latches                    ' Gets all latches from tables
                    Call Send_ADF                       ' Send Data
                    Call Check_Lock                     ' Check for Lock                    
                    DelayMS 100
                    Call UPDATE_GUI
                 Next LOOP16                   
                     
                                              
            Case    "T"            ' will pass "T XX XX XX XX" Where XX is ASCII Hex
                OFFSET = 0          ' custom request so offset is blown up
                Call HexConvert    ' Converts and Fills DTX_ByteCnt with the number of bytes  
                Temp = 0    
                'HRSOut 13,"Bytes = ",Dec2 DTX_ByteCnt, " HEX:"          
                While DTX_ByteCnt > 0
                    'HRSOut Hex2 DTX_Buff[Temp]
                    Inc Temp
                    Dec DTX_ByteCnt
                    'HRSOut ","
                Wend
                HRSOut 13   
                F_Latch = 0
                R_Latch = 0
                N_Latch = 0
                F_Latch.Byte0 = DTX_Buff[2]
                F_Latch.Byte1 = DTX_Buff[1]
                F_Latch.Byte2 = DTX_Buff[0]                                
                R_Latch.Byte0 = DTX_Buff[5]
                R_Latch.Byte1 = DTX_Buff[4]
                R_Latch.Byte2 = DTX_Buff[3] 
                N_Latch.Byte0 = DTX_Buff[8]
                N_Latch.Byte1 = DTX_Buff[7]
                N_Latch.Byte2 = DTX_Buff[6]                
                'HRSOut "F_LATCH = $", Hex6 F_Latch,13
                'HRSOut "R_LATCH = $", Hex6 R_Latch,13
                'HRSOut "N_LATCH = $", Hex6 N_Latch,13                               
                Call Send_ADF
                Call Check_Lock   
                
            Case "X"        'Power off RF
                PA_ON = 0
                PA_SW = 0
                VCO_SW = 0
                'HRSOut "Power Off",13
                HRSOut "{UI|SET|RF.Text=RF OFF}" 
                HRSOut "{UI|SET|PA.Text=PA OFF}" 
                HRSOut "{UI|SET|INFO.Text=VCO, PA AND DIGITAL ATTENUATOR OFF}"                  
                               
              
            Case "Y"        'Power on                           
                PA_ON = 1
                PA_SW = 1
                VCO_SW = 1
                'HRSOut "Power On",13 
                HRSOut "{UI|SET|RF.Text=RF ON}" 
                HRSOut "{UI|SET|PA.Text=PA ON}" 
                HRSOut "{UI|SET|INFO.Text=VCO, PA AND DIGITAL ATTENUATOR ON}"                 
                                                                                                               
            Case "z"   
                'HRSOut "PBI_TT",13
                HRSOut "{UI|SET|INFO.Text=MWAVE SOURCE V6.0 JULY 2019}"                
        End Select
  
out:        
        GoTo  New_Msg
        
    End   
    
N_Table:       ' table is 500 - 1000 entries every 0 or 5 will have a different FUNCTION ad REFERENCE  latch
'                        1                        5                            10                             15                           20
        LData As Dword $1901,$3E25,$1F15,$3E2D,$1F19,$1905,$1F1D,$3E3D,$1F21,$3F05,$1909,$3F0D,$1F29,$3F15,$1F2D,$190D,$1F31,$3F25,$1F35,$3F2D,$1911,_ '0000 to 0020  OK
                       $3F35,$1F3D,$3F3D,$2001,$1915,$2005,$400D,$2009,$4015,$1919,$401D,$2011,$4025,$2015,$191D,$2019,$4035,$201D,$403D,$1A01,_ '0021 to 0040  OK
                       $4105,$2025,$410D,$2029,$1A05,$202D,$411D,$2031,$4125,$1A09,$412D,$2039,$4135,$203D,$1A0D,$2101,$4205,$2105,$420D,$1A11,_ '0041 to 0060 OK 
                       $4215,$210D,$421D,$2111,$1A15,$2115,$422D,$2119,$4235,$1A19,$423D,$2121,$4305,$2125,$1A1D,$2129,$4315,$212D,$431D,$1B01,_ '0061 to 0080
                       $4325,$2135,$432D,$2139,$1B05,$213D,$433D,$2201,$4405,$1B09,$440D,$2209,$4415,$220D,$1B0D,$2211,$4425,$2215,$442D,$1B11,_ '0081 to 0100                        
                       $4435,$221D,$443D,$2221,$1B15,$2225,$450D,$2229,$4515,$1B19,$451D,$2231,$4525,$2235,$1B1D,$2239,$4535,$223D,$453D,$1C01,_ '0101 to 0120  
                       $4605,$2305,$460D,$2309,$1C05,$230D,$461D,$2311,$4625,$1C09,$462D,$2319,$4635,$231D,$1C0D,$2321,$4705,$2325,$470D,$1C11,_ '0121 to 0140   
                       $4715,$232D,$471D,$2331,$1C15,$2335,$472D,$2339,$4735,$1C19,$473D,$2401,$4805,$2405,$1C1D,$2409,$4815,$240D,$481D,$1D01,_ '0141 to 0160   
                       $4825,$2415,$482D,$2419,$1D05,$241D,$483D,$2421,$4905,$1D09,$490D,$2429,$4915,$242D,$1D0D,$2431,$4925,$2435,$492D,$1D11,_ '0161 to 0180   
                       $4935,$243D,$493D,$2501,$1D15,$2505,$4A0D,$2509,$4A15,$1D19,$4A1D,$2511,$4A25,$2515,$1D1D,$2519,$4A35,$251D,$4A3D,$1E01,_ '0181 to 0200   
                       $4B05,$2525,$4B0D,$2529,$1E05,$252D,$4B1D,$2531,$4B25,$1E09,$4B2D,$2539,$4B35,$253D,$1E0D,$2601,$4C05,$2605,$4C0D,$1E11,_ '0201 to 0220  
                       $4C15,$260D,$4C1D,$2611,$1E15,$2615,$4C2D,$2619,$4C35,$1E19,$4C3D,$2621,$4D05,$2625,$1E1D,$2629,$4D15,$262D,$4D1D,$1F01,_ '0221 to 0240  
                       $4D25,$2635,$4D2D,$2639,$1F05,$263D,$4D3D,$2701,$4E05,$1F09,$4E0D,$2709,$4E15,$270D,$1F0D,$2711,$4E25,$2715,$4E2D,$1F11,_ '0241 to 0260  
                       $4E35,$271D,$4E3D,$2721,$1F15,$2725,$4F0D,$2729,$4F15,$1F19,$4F1D,$2731,$4F25,$2735,$1F1D,$2739,$4F35,$273D,$4F3D,$2001,_ '0261 to 0280  
                       $5005,$2805,$500D,$2809,$2005,$280D,$501D,$2811,$5025,$2009,$502D,$2819,$5035,$281D,$200D,$2821,$5105,$2825,$510D,$2011,_ '0281 to 0300  
                       $5115,$282D,$511D,$2831,$2015,$2835,$512D,$2839,$5135,$2019,$513D,$2901,$5205,$2905,$201D,$2909,$5215,$290D,$521D,$2101,_ '0301 to 0320  
                       $5225,$2915,$522D,$2919,$2105,$291D,$523D,$2921,$5305,$2109,$530D,$2929,$5315,$292D,$210D,$2931,$5325,$2935,$532D,$2111,_ '0321 to 0340  
                       $5335,$293D,$533D,$2A01,$2115,$2A05,$540D,$2A09,$5415,$2119,$541D,$2A11,$5425,$2A15,$211D,$2A19,$5435,$2A1D,$543D,$2201,_ '0341 to 0360  
                       $5505,$2A25,$550D,$2A29,$2205,$2A2D,$551D,$2A31,$5525,$2209,$552D,$2A39,$5535,$2A3D,$220D,$2B01,$5605,$2B05,$560D,$2211,_ '0361 to 0380  
                       $5615,$2B0D,$561D,$2B11,$2215,$2B15,$562D,$2B19,$5635,$2219,$563D,$2B21,$5705,$2B25,$221D,$2B29,$5715,$2B2D,$571D,$2301,_ '0381 to 0400  
                       $5725,$2B35,$572D,$2B39,$2305,$2B3D,$573D,$2C01,$5805,$2309,$580D,$2C09,$5815,$2C0D,$230D,$2C11,$5825,$2C15,$582D,$2311,_ '0401 to 0420  
                       $5835,$2C1D,$583D,$2C21,$2315,$2C25,$590D,$2C29,$5915,$2319,$591D,$2C31,$5925,$2C35,$231D,$2C39,$5935,$2C3D,$593D,$2401,_ '0421 to 0440
                       $5A05,$2D05,$5A0D,$2D09,$1205,$2D0D,$5A1D,$2D11,$5A25,$2409,$5A2D,$2D19,$5A35,$2D1D,$240D,$2D21,$5B05,$2D25,$5B0D,$2411,_ '0441 to 0460 
                       $5B15,$2D2D,$5B1D,$2D31,$2415,$2D35,$5B2D,$2D39,$5B35,$2419,$5B3D,$2E01,$5C05,$2E05,$241D,$2E09,$5C15,$2E0D,$5C1D,$2501,_ '0461 to 0480 
                       $5C25,$2E15,$5C2D,$2E19,$1225,$2E1D,$5C3D,$2E21,$5D05,$2509,$5D0D,$2E29,$5D15,$2E2D,$250D,$2E31,$5D25,$2E35,$5D2D,$2511 '0481 to 0500 
                       '$,$,$,$,$,$,$,$,$,$,$,$,$,$,$,$,$,$,$,$,_ '0000 to 0000 
'                       $,$,$,$,$,$,$,$,$,$,$,$,$,$,$,$,$,$,$,$,_ '0000 to 0000 
'                       $,$,$,$,$,$,$,$,$,$,$,$,$,$,$,$,$,$,$,$,_ '0000 to 0000 
  
                                                                    
'         LData As Dword $0000,$0001,$0002,$0003,$0004,$0005,$0006,$0007,$0008,$0009,$000A    

                                                                                                                                                
CAL_TABLE:     ' Contains the Power LEVELING CALIBRATION
        LData As Byte 1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,4,4,4,4,4,4,4      
    
