. ************************************************************************
. *                                                                      *
. *  Include Name      :   PMSQVIFD.INC                                  *
. *                                                                      *
. *  Function          :   Queuing Visit Details Message File            *
. *                                                                      *
. *  File Type         :   SQL                                           *
. *                                                                      *
. *  File Name         :   pmsqviaf.dat                                  * 
. *                                                                      * 
. *  Keys              :   KEY22                                         *
. *                                                                      *
. *  Modifications     :                                                 *
. *                                                                      *
. ************************************************************************
. *       V11.05.01 20/11/2024 Thanh T        TSK 0951086                *
. *                 Changed adyhosp and ptmi3bdy size from 3 to 4        *
. ************************************************************************
. *       V11.04.01 19/01/2024 Thanh T        TSK 0936282                *
. *                 Changed AFNDMM size from 10 to 19                    *
. ************************************************************************
. *       V11.00.01 01.04.2020  J.Tan          TSK 0868813               *
. *                 Changed PKNAME from DIM30 to DIM80                   *
. *       V10.13.01 08/10/2018 Steve Armstrong  TSK 0860678              *
. *                 Added variables PMBRREQD to PTDSCLGP (including      *
. *                 moving PMBRREQD & PMRQREQT from SPARE into normal FD *
.*                  fields.                                              *
. ************************************************************************
. *       V10.12.01 20/06/2018 Steve Armstrong  TSK 0854259              *
. *                 Added new variables PMBRREQD & PMRQREQT.             *
. ************************************************************************
. *       V10.10.01 21/06/2017 Steve Armstrong                           *
. *                 Changed description for PTMSFHAN from "GPFH Approval *
. *                 Number" to "eAdmission ID".                          *
. ************************************************************************
. *               V10.08.01 01/04/2016 Steve Armstrong  TSK 0296379      *
. *                         Added new variables from PATTRNFD            *
. *                         (PTTRFSRC, PTTRHFND, PTTRFTBL, PTTRHFMN &    *
. *                         PTTRHFCN)                                    *
. ************************************************************************
. *               V10.06.02 28/07/2015 Steve Armstrong                   *
. *                         Fixed spelling mistakes in field             *
. *                         descriptions for ACANCEL and AELIG           *
. *               V10.05.01 12/08/2014  Steve Armstrong   CAR 304812     *
. *                         Extended AINVPRT from FORM2 to FORM3.        *
. *                         Also moved a number of fields and added      *
. *                         some new fields which have been added to the *
. *                         corresponding FD's.                          *
. *               V10.04.02 06/01/2014  Ania P            CAR 294384     *
. *                         Added PMVXCPDD and PMVXPBMI                  *
. *               V10.04.01 02/01/2014  Patrick Adair     CAR 295320     *
. *                         Moved ACLSSFT, ADOCTA & ATYPE out of SPARE   *
. ************************************************************************
. *               V10.03.02 18/07/2013  Steve Armstrong   CAR 268961     *
. *                         Added ACLSSFT, ADOCTA & ATYPE into SPARE     *
. *                         variable.                                    *
. *               V10.03.01 11/11/2011  Mike Laming       CAR 251323     *
. *                         Moved OERDATE, OERTIME, PTMILSDN, & PTMIOGNO *
. *                         out of Spare                                 *
. ************************************************************************
. *               V10.02.03 27/10/2011  Steve Armstrong   CAR 251323     *
. *                         Added PTMILSDN and PTMIOGNO as extra         *
. *                         variables.                                   *
. *               V10.02.02 12/10/2011  Steve Armstrong   CAR 251644     *
. *                         Added OERDATE and OERTIME as extra variables *
. *               V10.02.01 27.07.2011  Steve Armstrong   CAR 246136     *
. *                         Updated table to contain all the new fields  *
. *                         added to the relevant tables.                *
. ************************************************************************
. *               V10.01.01 23.11.2011  Steve Armstrong   CAR 233088     *
. *                         Added PMVXFNDM.                              *
. ************************************************************************
. *               V9.11.02  23.12.2008  Ebon Clements     CAR 184296     *
. *                         Changed PKNAME from 20 to 30 character       *
. *               V9.11.01  20/10/2008  Davin             CAR 175736     *
. *                         Added WTSBY and increased length to 1615     *
. ************************************************************************
. *               V9.10.01  04/06/2008  Ebon Clements     CAR 151025     *
. *                         Added PTREMOBL                               *
. ************************************************************************
. *               V9.05.B02 13/12/2005  Steve Armstrong   CAR 88619      *
. *                         Added ASTAT                                  *
. *               V9.05.B01 30/01/2006  Peter Vela        CAR 54614      *
. *                         Added PATRE1FD variables                     *
. ************************************************************************
. *                V9.04.02 30/08/2005  Davin   CAR 64087                *
. *                         Changed pattranf amounts to 12.2             *
. *                         Added PMQVTDAT & PMQVTTIM                    *
. *                         Increased spare and table length             *
. *                V9.04.01 09/05/2004  Davin   CAR 22680                *
. *                         Added PMVXRH1G,PMVXRH1C                      *
. *                V9.02.00 01/10/2001  Steve Armstrong                  *
. *                         FD Created                                   *
. *                                                                      *
. ************************************************************************
.
.Data File Definition
.--------------------
PMSQVIA1   IFILE SQL, FIXED=2789, KEY="U1-20,21-22"
.
.Name      Type   Length  Physical Start Description
.----      ----   ------  -------- ----- -----------
DPMQVMES   DIM      20       20       1  Message Id
DPMQVTRN   DIM       2        2      21  Transaction Number
.
.          PATMI1FD variables
.
.AADMNO    FORM      8        5      23  PATIENT ADMISSION NO.
.AURNO     DIM       8        8      28  PATIENT U.R NO.
.ADATE     DIM       8        8      36  Admission Date (CCYYMMDD)
.ATIME     DIM       8        8      44  PATIENT ADMISSION TIME
.ASTAT     FORM      1        2      52  Admission Status
.                                             1 = Pre-Admission
.                                             2 = Current Admission
.                                             3 = Discharged
.                                             4 = On Leave
.                                             5 = Cancelled
.AINVPRT   FORM      3        3      54  NUMBER OF INVOICES PRINTED
.ALACDTE   DIM       8        8      57  last accom date invd (CCYYMMDD)
.ACANCEL   DIM       3        3      65  REASON PRE-ADMISS CANCELATION
.ADOCTR    DIM       6        6      68  DOCTOR REFERRED BY.
.ADOCTA    DIM       6        6      74  Attending Doctor (patdo1af)
.ADOCTL    DIM      30       30      80  LOCAL DOCTOR
.ASRCE     DIM       3        3     110  PATIENT ADMISSIONS SOURCE  (CAT S )
.ATYPE     DIM       3        3     113  Admission Type (Cat A)
.ACLSS     DIM       3        3     116  PATIENT ADMISSIONS CLASS   (CAT P )
.ACARE     DIM       3        3     119  PATIENT CARE CLASS         (CAT CC)
.AFUNDH    DIM       6        6     122  PATIENT HEALTH FUND
.AFNDTB    DIM       8        8     128  PATIENT FUND TABLE
.AFNDMM    DIM      19       19     136  PATIENT FUND MEMBERSHIP NO
.AELIG     FORM      1        2     155  PATIENT ELIGIBILITY FOR GOV'T
.                                         REBATE. 1 = YES  2 = NO
.                                         STH - DISCHARGE DIAG.PRINTED
.AVISIT    FORM      1        2     157  PATIENT RELIGIOUS VISIT
.                                                 1 = YES, 2 = NO
.AALERG    FORM      1        2     159  PATIENT ALLERGIES (Y/N)
.                                                 1 = YES, 2 = NO
.AILLN     FORM      1        2     161  PRE-EXISTING ILLNESS
.                                                 1 = YES, 2 = NO
.ADIAG1    DIM      80       80     163  PATIENTS ADMITTING DIAG 1
.ADIAG2    DIM      80       80     243   "   "     "   "     "  2
.ADISC     FORM      6.2      5     323  PATIENT DISCOUNT PERCENTAGE
.ADOCTT    DIM       6        6     328  TREATING DOCTOR
.ACLSSFT   DIM       3        3     334  Department (Cat AC)
.AALLOW    DIM       2        2     337  PATIENT ALLOWANCE CODE
.AMBS      DIM       9        9     339  ADMITTING MBS CODE
.ACLAIM    DIM       3        3     348  CLAIM CODE (CAT CL)
.ADIET     DIM       3        3     351  DIET CODE (CAT DC)
.APLOCCR   DIM       3        3     354  PLACE OF OCCR (CATEGORY PO)
.ASTAY     FORM      3        3     357  EXPECTED LENGTH OF STAY (DAYS)
.ADYHOSP   FORM      4        3     360  No OF DAYS OF HOSPITALIZATION
.AMEMB     FORM      1        2     363  DIAMOND VALLEY MEMBERSHIP
.                                                 1 = YES, 2 = NO
.AMEMBNO   FORM      6        4     365  MEMBERSHIP NUMBER
.ACOMM1    DIM      40       40     369  COMMENTS FIELD 1
.ACOMM2    DIM      30       30     409  COMMENTS FIELD 2
.APLUR     FORM      1        2     439  PLURATILY OF BABY (0=NORMAL)
.APURGE    FORM      1        2     441  PURGE INDICATOR
.                                                 OTHER = NO PURGE
.                                                 1     = PARTIAL
.                                                 2     = FULLY
.AUSR1     DIM       3        3     443  USER DEFINED FIELD 1 (CAT U1)
.AUSR2     DIM       3        3     446  USER DEFINED FIELD 2 (CAT U2)
.AUSR3     DIM       3        3     449  USER DEFINED FIELD 3 (CAT U3)
.AUSR4     DIM       3        3     452  USER DEFINED FIELD 4 (CAT U4)
.AUSR5     DIM       3        3     455  USER DEFINED FIELD 5 (CAT U5)
.AUSR6     DIM       3        3     458  USER DEFINED FIELD 6 (CAT U6)
.                                                 - Clinic Unit No. for Publics
.AUSR7     DIM       3        3     461  USER DEFINED FIELD 7 (CAT U7)
.                                                 - Coding status for Publics
.ADSCHI    FORM      1        2     464  DISCHARGE INVOICE INDICATOR
.                                                 0 = Discharge inv not printed
.                                                 1 = Have printed discharge inv
.ARDRNAM   DIM      22       22     466  FREE FORMAT REFERRING DOCTOR
.AFNDYY    DIM       2        2     488  YRS OF FUND MEMBERSHIP (JR,DE)
.AFNDCG    DIM       6        6     490  fund cover change (CCYYMM)
.AOCCDTE   DIM       8        8     496  DATE OF OCCURRENCE (CCYYMMDD)
.ACODDTE   DIM       8        8     504  DATE CODING COMPLETED
.ARETDTE   DIM       8        8     512 EXPECTED RETURN DATE FROM LVE
.AMUMADM   FORM      8        5     520  MOTHERS ADMSS NUMBER(IF BABY)
.PTMIPVIS  FORM      3        3     525  No of prev. visits charged
.ABWGHT    DIM       6        6     528  Baby's /Admission weight
.PTMSRFGP  DIM       8        8     534  Referring GP.
.PTMSGPPC  DIM       6        6     542  GP Practice Code
.PTMSFHAN  DIM      20       20     548  eAdmission ID
.PTMSECRA  DIM      20       20     568  ECR Approval Number
.PTMSMGIN  DIM       3        3     588  Management Intent   (CAT MI)
.PTMIDDIG  FORM      1        2     591  Discharged diagnosis printed?
.PTMIGPPT  DIM       2        2     593  GP Practice count
.PTMIDOFR  DIM       3        3     595  District of Residence (Cat DA)
.PTMIREFD  DIM       8        8     598  Date of Referral (EDI nz)
.PTMIDFCN  DIM       8        8     606  Date of 1st consultation (EDI nz)
.PTMIPHPU  DIM       3        3     614  Principal Health Service Purchaser
.                                         (EDI nz) (CAT HP)
.PTMIAGNC  DIM       3        3     617  Health Agency (EDI nz) (CAT HA)
.PTMIESSF  DIM       1        1     620  Event Summary supress flag Y/N(EDI nz)
.                                         "Y" = Suppress event summary
.                                         "N" = Don't supress
.PTMIUSR8  DIM       3        3     621  User defined field 8 (CAT U8)
.PTMIUSR9  DIM       3        3     624  User defined field 9 (CAT U9)
.PTMIUSR0  DIM       3        3     627  User defined field 0 (CAT U0)
.PTMIPDRG  DIM       4        4     630  Provisional DRG
.PTMIOPER  DIM       4        4     634  Operator who pre-admitted patient
.PTMIXWRD  DIM       3        3     638  Expected Ward for Pre-Admission
.PTMIADOC  DIM       6        6     641  Associated Doctor
.PTMIREGS  DIM       6        6     647  Registrar
.PTMIUYN1  FORM      1        2     653  User Defined Y/N Field (1=Yes, 0=No)
.PTMIUYN2  FORM      1        2     655  User Defined Y/N Field (1=Yes, 0=No)
.PTMIPANO  DIM       8        8     657  Previous admission number 
.PTMIPADT  DIM       8        8     665  Previous admission date (ccyymmdd)
.PTMIPCMX  DIM       9        9     673  Casemix Code (Provisional MBS/DRG)
.PTMIGSTA  DIM       1        1     682  Is GST applicable to this event
.                                         Y=Yes, N/Space=No, U=Unknown
.PTMICMXP  DIM       1        1     683  Casemix Payment (Y/N)
.                                         NZ Billing - FFS has been charged(Y/N)
.PTMIEBED  DIM       3        3     684  Expected Bed
.PTMIDMOV  DIM       1        1     687  Pre-admission movement indicator
.PTMIDFAS  DIM       8        8     688  Date Patient First Aware of Symptoms
.PTMICRCD  DIM       1        1     696  Credit Card Details on File (1=Yes)
.PTMIFINV  DIM       1        1     697  Follow-up for implant invoicing(1=Yes)
.PTMICONB  DIM       1        1     698  Continuous Billing Event (1=Yes)
.PTMIDATC  DIM       8        8     699  Date Created CCYYMMDD
.PTMITIMC  DIM       8        8     707  Time Created hh:mm:ss
.PTMIWEBC  DIM      10       10     715  Web user Id Created
.PTMIDATU  DIM       8        8     725  Date Updated CCYYMMDD
.PTMITIMU  DIM       8        8     733  Time Updated hh:mm:ss
.PTMIWEBU  DIM      10       10     741  Web user Id Updated
.PTMI3BDY  DIM       4        4     751  3B certificate days
.PTMI3BAD  DIM       8        8     755  3B certificate admission date
.PTMILSDN  DIM       8        8     763  Linked Statistical Discharge Number
.PTMIOGNO  DIM       8        8     771  Original Admission Number
.
.         PATTRNFD variables
.
.TWARD     DIM       3        3     779  TRANSFER WARD NUMBER
.TBED      DIM       3        3     782  TRANSFER BED NUMBER
.TDATE     DIM       8        8     785  TRANSFER DATE (CCYYMMDD)
.TTIME     DIM       8        8     793  TRANSFER TIME (HH:MM)
.TURNO     DIM       8        8     801  PATIENT U/R  NUMBER
.TADMN     FORM      8        5     809  PATIENT ADMISSION  NUMBER
.TRATEF    FORM      12.2     8     814  PAT PORTION OF DAILY CHARGE
.                                             Daily rate = TRATEF + TRATEH
.                                                          - TDISC - TALLW
.                                          Accommodation Charge - Nursing Home
.TRATEG    FORM      12.2     8     822  GOVERNMENT  CHARGEABLE
.TDISC     FORM      12.2     8     830  PAT DISC (SUB FROM TRATEF)
.TALLW     FORM      12.2     8     838  PAT ALLOW (SUB FROM TRATEF)
.TATYPE    DIM       3        3     846  ADMISSION TYPE
.TADOCT    DIM       6        6     849  ATTENDING DOCTOR
.TRATEH    FORM      12.2     8     855  H/FND PORTION OF DAILY CHARGE
.TEXTRA    FORM      12.2     8     863  EXTRA CHGE PER DAY (PUB HOSP)
.PTTRITFS  FORM      12.2     8     871  Nursing Home Income Tested fee
.TRBTYP    DIM       3        3     879  E.F.R. BED TYPE FOR THIS BED
.TRBEND    FORM      3        3     882  END DAY IN RNGE FOR THIS RATE
.TCHSTAT   DIM       3        3     885  CHRGEABLE STATUS (USER DEF.2)
.                                          (PUBLIC HOSPITAL ONLY)
.TDEPT     DIM       3        3     888  DEPARTMENT CODE (CAT AC)
.PTTRLTYP  DIM       3        3     891  Type of Patient leave (Cat TL)
.PTTREPNO  FORM      2        2     894  Episode number
.PTTREPSD  FORM      1        2     896  Manual Stepdown
.                                           0 - Normal
.                                           1 - Forced Stepdown
.                                           2 - Bed Fee Update
.PTTRLSPT  FORM     12.2      8     898  Patient portion of lumpsum
.PTTRLSRB  FORM     12.2      8     906  Rebate portion of lumpsum
.PTTRAEND  FORM      4        3     914  End day for additional rate
.PTTRADPT  FORM     12.2      8     917  Additional daily rate - pat.
.                                          Resident contribution - Nursing Home
.PTTRADRB  FORM     12.2      8     925  Additional daily rate - rebate
.PTTRNHAC  DIM        3       3     933  Nursing Home Account code (Catg NH)
.PTTROPER  DIM        4       4     936  Operator Id
.PTTRAUAT  DIM        1       1     940  Auto update of admission type
.PTTRCDAT  DIM        8       8     941  Date Record Created (CCYYMMDD)
.PTTRCTIM  DIM        8       8     949  Time Record Created (hh:mm:ss)
.PTTRCUID  DIM        4       4     957  Passcode Created
.PTTRLTSC  DIM        5       5     961  Leave Transfer Source
.PTTRUCID  DIM       10      10     966  Web User Id who Created Record
.PTTRUUID  DIM       10      10     976  Web User Id who Updated Record
.PTTRCLTY  DIM        3       3     986  Claim Type (Cat CL)
.PTTRTRTY  DIM        3       3     989  Transfer Type (Cat RB)
.PTTRTEAM  DIM        5       5     992  Team
.PTTRRSTR  DIM       10      10     997  Registrar
.PTTRRESI  DIM       10      10    1007  Resident
.PTTRFSRC  DIM       3       3     1017  Funding Source (Cat cx)
.PTTRHFND  DIM       6       6     1020  Health Fund Code (patfn1af)
.PTTRFTBL  DIM       8       8     1026  Health Fund Table (patfn1af)
.PTTRHFMN  DIM       19      19    1034  Health Fund Membership Number
.PTTRHFCN  DIM       40      40    1053  Health Fund Contributor Name
.
.         PMSVX1FD variables
.
.PMVXCSNR  DIM       1        1    1093  Consent release info (0=NO,1=YES)
.PMVXPOST  DIM       8        8    1094  Post Code
.PMVXPIND  DIM       3        3    1102  Privacy Indicator (Cat PV)
.PMVXVLOC  DIM      40       40    1105  Valuables Location
.PMVXCFSG  DIM       1        1    1145  Claim Form Signed (0=NO,1=YES)
.PMVXINGP  DIM       1        1    1146  Inform Registered GP
.                                         (0=NO,1=YES,2=Not Stated)
.PMVXCPTH  DIM       3        3    1147  Critical Pathway (Cat PW)
.PMVXHCP1  DIM       1        1    1150  Notify HCP1 (0=NO,1=YES)
.PMVXHCP2  DIM       1        1    1151  Notify HCP2 (0=NO,1=YES)
.PMVXHCP3  DIM       1        1    1152  Notify HCP3 (0=NO,1=YES)
.PMVXHCP4  DIM       1        1    1153  Notify HCP4 (0=NO,1=YES)
.PMVXRHC1  DIM      10       10    1154  Referring HCP 1 (pmshcpaf)
.PMVXRH1G  DIM      10       10    1164  Referring HCP 1 Practice (pmshclaf)
.PMVXRH1C  DIM       2        2    1174  Referring HCP 1 Grp Count (pmshcgaf
.PMVXRHC2  DIM      10       10    1176  Referring HCP 2 (pmshcpaf)
.PMVXRH2G  DIM      10       10    1186  Referring HCP 2 Practice (pmshclaf)
.PMVXRH2C  DIM       2        2    1196  Referring HCP 2 Grp Count (pmshcgaf
.PMVXRHC3  DIM      10       10    1198  Referring HCP 3 (pmshcpaf)
.PMVXRH3G  DIM      10       10    1208  Referring HCP 3 Practice (pmshclaf)
.PMVXRH3C  DIM       2        2    1218  Referring HCP 3 Grp Count (pmshcgaf
.PMVXRHC4  DIM      10       10    1220  Referring HCP 4 (pmshcpaf)
.PMVXRH4G  DIM      10       10    1230  Referring HCP 4 Practice (pmshclaf)
.PMVXRH4C  DIM       2        2    1240  Referring HCP 4 Grp Count (pmshclaf
.PMVXABRG  DIM       3        3    1242  Aboriginality (PRS/2) (Cat VA)
.PMVXCADM  DIM       3        3    1245  Admission Criterion (PRS/2) (Cat VB
.PMVXIRAD  DIM       3        3    1248  Intent to Readmit (PRS/2) (Cat VR)
.PMVXIDUS  DIM       3        3    1251  Intend Stay Duration (PRS/2) Cat VI
.PMVXCRAV  DIM       3        3    1254  Carer Availability (PRS/2) (Cat VK)
.PMVXRCCT  DIM       3        3    1257  Crit. Care Xfer Reason (PRS/2) (VJ)
.PMVXAPWD  DIM       3        3    1260  Admitting Point Ward (patwr1af)
.PMVXAPBD  DIM       3        3    1263  Admitting Point Bed (patwr1af)
.PMVXPOWD  DIM       3        3    1266  Post-Op Ward (patwr1af)
.PMVXPOBD  DIM       3        3    1269  Post-Op Bed (patwr1af)
.PMVXPSTY  DIM       1        1    1272  Parent Staying (0=NO,1=YES)
.PMVXVALW  DIM       1        1    1273  Visitor Allowed (0=NO,1=YES)
.PMVXPALW  DIM       1        1    1274  Phone Calls Allowed? (0=NO,1=YES)
.PMVXINDC  DIM       1        1    1275  Intended Day Case (0=NO,1=YES)
.PMVXMDAV  DIM       3        3    1276  Mode of Arrival (Cat VM)
.PMVXFRMS  DIM       3        3    1279  Forms (Cat VF)
.PMVXCSNG  DIM       3        3    1282  Coder Snags (Cat VN)
.PMVXPWGT  FORM      6        4    1285  Patient Weight(Convert to FORM 4.2)
.PMVXPHGT  FORM      6        4    1289  Patient Height
.PMVXDHWR  DIM       8        8    1293  Date Height/Weight Record. ccyymmdd
.PMVXEDC1  DIM      10       10    1301  Extra Doctor Code 1 (patdo1af)
.PMVXEDC2  DIM      10       10    1311  Extra Doctor Code 2 (patdo1af)
.PMVXEDC3  DIM      10       10    1321  Extra Doctor Code 3 (patdo1af)
.PMVXHOME  DIM       1        1    1331  Homeless Person (0=NO,1=YES)
.PMVXUDF1  DIM      50       50    1332  User Defined Free Format 1
.PMVXUDF2  DIM      50       50    1382  User Defined Free Format 2
.PMVXUDF3  DIM      50       50    1432  User Defined Free Format 3
.PMVXUDF4  DIM      50       50    1482  User Defined Free Format 4
.PMVXUDT1  DIM      13       13    1532  User Def. Date/Time 1 ccyymmddhh:mm
.PMVXUDT2  DIM      13       13    1545  User Def. Date/Time 2 ccyymmddhh:mm
.PMVXUDT3  DIM      13       13    1558  User Def. Date/Time 3 ccyymmddhh:mm
.PMVXUDT4  DIM      13       13    1571  User Def. Date/Time 4 ccyymmddhh:mm
.PMVXUDT5  DIM      13       13    1584  User Def. Date/Time 5 ccyymmddhh:mm
.PMVXUDT6  DIM      13       13    1597  User Def. Date/time 6 ccyymmddhh:mm
.PMVXCDTE  DIM      8        8     1610  Date record created (ccyymmdd)
.PMVXCTIM  DIM      8        8     1618  Time record created (hh:mm:ss)
.PMVXWEBC  DIM      10       10    1626  WEB User Id rec. creator (websecaf)
.PMVXLUPD  DIM      8        8     1636  Last Update Date (ccyymmdd)
.PMVXLUPT  DIM      8        8     1644  Last Update Time (hh:mm:ss)
.PMVXWEBU  DIM      10       10    1652  WEB User Id rec. update (websecaf)
.PMVXMHLS  DIM      3        3     1662  Mental Health Legal Status (LS)
.PMVXPICD  DIM      9        9     1665  Provisional ICD10 Code
.PMVXRMOR  DIM      3        3     1674  RMO Responsible Cat MO
.PMVXASUT  DIM      3        3     1677  Admission Service Unit Type Cat KA
.PMVXELPS  DIM      3        3     1680  Elective Patient Status Cat KC
.PMVXEMPL  DIM      3        3     1683  Employment Status Cat KD
.PMVXFINP  DIM      3        3     1686  Which Financial Program Cat KE
.PMVXOCCP  DIM      3        3     1689  Occupation Cat KF
.PMVXPALC  DIM      3        3     1692  Involvement in Palliat Care Cat KG
.PMVXPPAL  DIM      3        3     1695  Prev Non-Admitt Palliat Care Cat KH
.PMVXPPPT  DIM      3        3     1698  Public/Private Patients Cat KI
.PMVXPSOS  DIM      3        3     1701  Pension Status on Separation Cat KJ
.PMVXPYSP  DIM      3        3     1704  Payment Status on Separation Cat KK
.PMVXQLDB  DIM      3        3     1707  Banding/Type of Episode for QLDHDP
.PMVXREAD  DIM      3        3     1710  Re-Admission in 28 Days Cat KM
.PMVXUDIN  DIM      3        3     1713  Discharge Intention Cat KN
.PMVXUREA  DIM      3        3     1716  Unplanned Readmission Cat KO
.PMVXUSAC  DIM      3        3     1719  Usual Accommodation Cat KP
.PMVXUVTH  DIM      3        3     1722  Unplanned Visit to Theatre Cat KB
.PMVXINTR  DIM      1        1     1725  Interpreter Required? (0=NO,1=YES)
.PMVXLNG1  DIM      3        3     1726  Preferred Language 1 (Cat LA)
.PMVXASGC  DIM      9        9     1729  ABS AGSC
.PMVXMHOS  DIM      3        3     1738  Multi Hospital ID (pathspaf)
.PMVXDSPD  DIM      8        8     1741  Disch Summary Produced (ccyymmdd)
.PMVXDSIF  DIM      1        1     1749  Disch Summary (I)nterim / (F)inal
.PMVXPRMI  DIM      1        1     1750  Participate/Receive Marketing Info
.PMVXPRAI  DIM      1        1     1751  Participate/Receive Advertising Inf
.PMVXPRES  DIM      1        1     1752  Participate in Research Projects
.PMVXUITR  DIM      1        1     1753  Use Information for Training
.PMVXRPOA  DIM      1        1     1754  Release to Enduring POA / IFC
.PMVXRCN1  DIM      1        1     1755  Release to Contact 1 ptmxnrnm
.PMVXRCN2  DIM      1        1     1756  Release to Contact 2 ptmxexnm
.PMVXRHFU  DIM      1        1     1757  Release to Health Fund
.PMVXRDVA  DIM      1        1     1758  Release to DVA
.PMVXRSLO  DIM      1        1     1759  RSL Visit OK
.PMVXMLOC  DIM      3        3     1760  Medical Record Location (Cat MV)
.PMVXPACC  DIM      3        3     1763  Preferred Accommodation (Cat BP)
.PMVXEDU1  DIM      3        3     1766  Extra Doctor Unit 1 (Cat AC)
.PMVXEDU2  DIM      3        3     1769  Extra Doctor Unit 2 (Cat AC)
.PMVXEDU3  DIM      3        3     1772  Extra Doctor Unit 3 (Cat AC)
.PMVXPRGM  DIM      8        8     1775  Program Code
.PMVXCONF  DIM      1        1     1783  Appointment Confirmed 0=No 1=Yes
.PMVXEOCL  DIM      8        8     1784  EOC Link admission no.
.PMVXSLOC  DIM      3        3     1792  Location SOR (Cat SG)
.PMVXSCLI  DIM      3        3     1795  Clinical SOR (Cat SH)
.PMVXSTRA  DIM      3        3     1798  Transport SOR (Cat.SI)
.PMVXQASN  DIM      12       12    1801  QAS Number
.PMVXATCP  DIM      1        1     1813  All theatre charges posted (1=yes)
.PMVXATIM  DIM      8        8     1814  Arrival Time (hh:mm:ss)
.PMVXITIM  DIM      8        8     1822  Interview Time (hh:mm:ss)
.PMVXHFGN  DIM      40       40    1830  Health Fund First Given Name Alias
.PMVXHFSN  DIM      40       40    1870  Health Fund Surname Alias
.PMVXFUPI  DIM      2        2     1910  Health Fund UPI
.PMVXPOEC  DIM      1        1     1912  Patient Online Eligibility Consent
.PMVXPGID  DIM      3        3     1913  Program Identifier (Cat PK)
.PMVXTICM  DIM      3        3     1916  Reason Theatre Incomplete Catg Rt
.PMVXPILC  DIM      3        3     1919  Presenting Illness Code CAT (il)
.PMVXFVIO  DIM      3        3     1922  Family Violence (Cat fv)
.PMVXFNDM  DIM      19       19    1925  Health Fund Membership Number
.PMVXUDF5  DIM      3        3     1944  User defined Category Code
.PMVXUDF6  DIM      3        3     1947  User defined Category Code
.PMVXUDF7  DIM      3        3     1950  User defined Category Code
.PMVXUDF8  DIM      3        3     1953  User defined Category Code
.PMVXUDF9  DIM      3        3     1956  User defined Category Code
.PMVXUD10  DIM      3        3     1959  User defined Category Code
.PMVXUD11  DIM      3        3     1962  User defined Category Code
.PMVXUD12  DIM      3        3     1965  User defined Category Code
.PMVXUD13  DIM      3        3     1968  User defined Category Code
.PMVXUD14  DIM      3        3     1971  User defined Category Code
.PMVXUD15  DIM      3        3     1974  User defined Category Code
.PMVXTEAM  DIM      5        5     1977  Team
.PMVXNWHC  DIM      3        3     1982  Non WA Hospital Contact (Cat cw)
.PMVXEDDT  DIM      8        8     1985  Expected Delivery Date (ccyymmdd)
.PMVXPHLD  DIM      3        3     1993  Property Held (Cat ps)
.PMVXPLOC  DIM      3        3     1996  Property Location (Cat pt)
.PMVXDTRT  DIM      8        8     1999  Date Returned (ccyymmdd)
.PMVXDFLG  DIM      9        9     2007  Disaster Flag
.PMVXFSRC  DIM      3        3     2016  Funding Source (Cat cx)
.PMVXCONM  DIM      40       40    2019  Contributor Name
.PMVXCPDD  DIM       8       8     2059  Care Plan Documented Date
.PMVXPBMI  DIM       5       5     2067  Patient Body Mass Index
.
.          PATDSCFD variables
.
.DDATE     DIM       8        8    2072   PATIENT DIS. DATE (CCYYMMDD)
.DTIME     DIM       8        8    2080   PATIENT DISCHARGE TIME
.DSTAT     DIM       3        3    2088   DISCHARGE STATUS      (CAT D )
.DDEST     DIM       3        3    2091   DISCHARGE DESTINATION (CAT DD)
.DDIAG     DIM      50       50    2094   DISCHARGE DIAGNOSIS
.DDIAG2    DIM      50       50    2144   DISCHARGE DIAGNOSIS (PART 2)
.DUSD1     DIM       3        3    2194   USER DEFINED FIELD 1 (CAT D1)
.DUSD2     DIM       3        3    2197   USER DEFINED FIELD 2 (CAT D2)
.DUSD3     DIM       3        3    2200   USER DEFINED FIELD 3 (CAT D3)
.DUSD4     DIM       3        3    2203   USER DEFINED FIELD 4 (CAT D4)
.DUSD5     DIM       3        3    2206   USER DEFINED FIELD 5 (CAT D5)
.DFMBS     DIM       9        9    2209   DISCHARGE DIAGNOSIS CODE
.PTDSDMDC  DIM       4        4    2218   MDC code
.PTDSDDRG  DIM       4        4    2222   DRG code
.DWLREASN  DIM       3        3    2226   Reason Discharged Patient was
.                                                put back on Waiting List (RW)
.PTDSSIDX  DIM       1        1    2229   DRG severity index
.PTDSVOGU  DIM       3        3    2230   Vers grouper used when grouping
.PTDSOPER  DIM       4        4    2233   Operator who discharged patie
.PTDSUYN1  FORM      1        2    2237   User Defined Y/N Field 1
.PTDSUYN2  FORM      1        2    2239   User Defined Y/N Field 2
.PTDSUYN3  FORM      1        2    2241   User Defined Y/N Field 3
.PTDSUYN4  FORM      1        2    2243   User Defined Y/N Field 4
.PTDSDWRD  DIM       3        3    2245   Discharge Ward
.PTDSDLOS  FORM      4        3    2248   Length of Stay     
.PTDSSREF  DIM       4        4    2251   Separation referral
.PTDSPALD  DIM       3        3    2255   Palliative care days (PRS2)
.PTDSDPMN  DIM       3        3    2258   Departure Means (Cat SI)
.
.          MEHLERFD variables
.
.MHLRCODE  DIM       3        3    2261  Leave end reason code (CAT LE)
.
.          PATRE1FD variables
.
.PKNAME    DIM      80       80    2264   PERSON RESP FOR A/C NAME
.PKADD1    DIM      35       35    2344   P.R.A ADDRESS LINE 1
.PKADD2    DIM      35       35    2379   P.R.A ADDRESS LINE 2
.PKSUBR    DIM      35       35    2414   P.R.A SUBURB
.PKADD4    DIM      35       35    2449   P.R.A ADDRESS LINE 2
.PKPOST    DIM       8        8    2484   P.R.A POSTCODE
.PKTELEP   DIM      20       20    2492   P.R.A PRIVATE TELEPHONE
.PKTELEB   DIM      20       20    2512   P.R.A BUSINESS TELEPHONE
.PKRELP    DIM      10       10    2532   P.R.A RELATIONSHIP
.PTREMOBL  DIM      20       20    2542   P.R.A MOBILE TELEPHONE
.PTRESNAM  DIM      40       40    2562   Surname or
.                                             Contact Name for Insurance
.PTREGNAM  DIM      40       40    2602   Given Name
.
.          Other variables
.
.WSTBY     FORM     8        5     2642   Standby Number
PMQVTDAT   DIM      8        8     2647   Transfer Date (for cancelled transfer)
.                                                       (ccyymmdd)
PMQVTTIM   DIM      8        8     2655   Transfer Date (for cancelled transfer)
.                                                       (hh:mm:ss)
.
.OERDATE   DIM      8        8     2663   Expected Return Date (ccyymmdd)
.OERTIME   DIM      8        8     2671   Expected Return Time (hh:mm:ss)
.
.PMBRREQD  DIM      8        8     2679   Request Date (ccyymmdd)
.PMBRREQT  DIM      8        8     2687   Request Time (hh:mm:ss)
.PMVXCNID  DIM      10      10     2695   Visit Confirmed WEB User Id
.                                         (websecaf)
.PMVXCNDT  DIM      13      13     2705   Visit Confirmed Date and Time
.                                         (ccyymmddhh:mm)
.PMVXCASE  DIM      10      10     2718   Case Team (allcasaf)
.PMVXEAFL  DIM      1        1     2728   Eclipse Adjustment Flag
.                                         Blank or 0 = No
.                                         1 = Yes
.PTDSLSDN  DIM      8        8     2729   Linked Statistical Discharge No
.PTDSCLGP  DIM     12       12     2737   Clinical Group
PMQVSPAR   DIM     40       40     2749   Spare Variable
.
.End of Record                     2789
.
.
. Redefined FORM fields from key
. ------------------------------
PMQVMESI  FORM      20
PMQVTRAN  FORM      2
