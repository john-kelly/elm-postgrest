create table school (
    unitid text primary key,
    instnm text not null,
    addr text not null,
    city text not null,
    stabbr text not null,
    zip text not null,
    webaddr text not null,
    longitud text not null,
    latitude text not null
);

/*
UNITID	    N	6	Cont		Unique identification number of the institution
INSTNM	    A	120	Alpha		Institution (entity) name
ADDR	    A	100	Alpha		Street address or post office box
CITY	    A	30	Alpha		City location of institution
STABBR	    A	2	Disc		State abbreviation
ZIP	        A	10	Alpha		ZIP code
FIPS	    N	2	Disc		FIPS state code
OBEREG	    N	2	Disc		Bureau of Economic Analysis (BEA) regions
CHFNM	    A	50	Alpha		Name of chief administrator
CHFTITLE	A	50	Alpha		Title of chief administrator
GENTELE	    A	15	Alpha		General information telephone number
EIN	        A	9	Cont		Employer Identification Number
OPEID	    A	8	Cont		Office of Postsecondary Education (OPE) ID Number
OPEFLAG	    N	1	Disc		OPE Title IV eligibility indicator code
WEBADDR	    A	150	Alpha		Institution's internet website address
ADMINURL	A	200	Alpha		Admissions office web address
FAIDURL	    A	200	Alpha		Financial aid office web address
APPLURL	    A	200	Alpha		Online application web address
NPRICURL	A	200	Alpha		Net price calculator web address
VETURL	    A	200	Alpha		Veterans and Military Servicemembers tuition policies web address
ATHURL	    A	150	Alpha		Student-Right-to-Know student athlete graduation rate web address
SECTOR	    N	2	Disc		Sector of institution
ICLEVEL	    N	2	Disc		Level of institution
CONTROL	    N	2	Disc		Control of institution
HLOFFER	    N	2	Disc		Highest level of offering
UGOFFER	    N	2	Disc		Undergraduate offering
GROFFER	    N	2	Disc		Graduate offering
HDEGOFR1	N	2	Disc		Highest degree offered
DEGGRANT	N	2	Disc		Degree-granting status
HBCU	    N	2	Disc		Historically Black College or University
HOSPITAL	N	2	Disc		Institution has hospital
MEDICAL	    N	2	Disc		Institution grants a medical degree
TRIBAL	    N	2	Disc		Tribal college
LOCALE	    N	2	Disc		Degree of urbanization (Urban-centric locale)
OPENPUBL	N	2	Disc		Institution open to the general public
ACT	        A	1	Disc		Status of institution
NEWID	    N	6	Cont		UNITID for merged schools
DEATHYR	    N	4	Disc		Year institution was deleted from IPEDS
CLOSEDAT	A	10	Alpha		Date institution closed
CYACTIVE	N	1	Disc		Institution is active in current year
POSTSEC	    N	2	Disc		Primarily postsecondary indicator
PSEFLAG	    N	2	Disc		Postsecondary institution indicator
PSET4FLG	N	2	Disc		Postsecondary and Title IV institution indicator
RPTMTH	    N	2	Disc		Reporting method for student charges, graduation rates, retention rates and student financial aid
IALIAS	    A	2000	Alpha		Institution name alias
INSTCAT	    N	2	Disc		Institutional category
C15BASIC	N	2	Disc		Carnegie Classification 2015: Basic
C15IPUG	    N	2	Disc		Carnegie Classification 2015: Undergraduate Instructional Program
C15IPGRD	N	2	Disc		Carnegie Classification 2015: Graduate Instructional Program
C15UGPRF	N	2	Disc		Carnegie Classification 2015: Undergraduate Profile
C15ENPRF	N	2	Disc		Carnegie Classification 2015: Enrollment Profile
C15SZSET	N	2	Disc		Carnegie Classification 2015: Size and Setting
CCBASIC	    N	2	Disc		Carnegie Classification 2005/2010: Basic
CARNEGIE	N	2	Disc		Carnegie Classification 2000
LANDGRNT	N	2	Disc		Land Grant Institution
INSTSIZE	N	2	Disc		Institution size category
CBSA	    N	5	Disc		Core Based Statistical Area (CBSA)
CBSATYPE	N	2	Disc		CBSA Type Metropolitan or Micropolitan
CSA	        N	3	Disc		Combined Statistical Area (CSA)
NECTA	    N	5	Disc		New England City and Town Area (NECTA)
F1SYSTYP	N	2	Disc		Multi-institution or multi-campus organization
F1SYSNAM	A	80	Alpha		Name of multi-institution or multi-campus organization
F1SYSCOD	A	6	Disc		Identification number of multi-institution or multi-campus organization
COUNTYCD	N	5	Disc		Fips County code
COUNTYNM	A	30	Alpha		County name
CNGDSTCD	N	4	Disc		State and 114TH Congressional District ID
LONGITUD	N	12	Cont		Longitude location of institution
LATITUDE	N	12	Cont		Latitude location of institution
DFRCGID	    N	3	Disc		Data Feedback Report comparison group category created by NCES
DFRCUSCG	N	2	Disc		Data Feedback Report - Institution submitted a custom comparison group
*/
