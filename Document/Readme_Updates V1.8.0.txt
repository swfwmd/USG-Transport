USG-Transport Version 1.1.0 - Updates to USG-Transport Code and documentation:
----------------------------  ------------------------------------------------

Updated CLN input structure: (changed hardwired input to read as an OPTION to be bacaward compatible with MF-USG files). Also included output formatting for transport in CLNs; and included rectangular type CLNs.
------------------------------------------------------------------------------------------------
 
Added capabilities and/or input instructions for the following packages: 

	Transient IBOUND (TIB) Package updated for transport in GLO2BASU1
	Updated TVM package of Damian Merrick to include transport parameters
	Recharge (RCH) Package with Transport - include non-zero inflow concentrations 
	Segmented Evapotranspiration (ETS) Package with Transport
	Drain with Return Flow (DRT) Package
	Sink with Return Flow (QRT) Package 
	Added "SOLVEACTIVE" option in Sparse Matrix Solver (SMS) Package to only send active nodes to linear solvers. 
	Added "DAMPBOT" option in Sparse Matrix Solver (SMS) Package (similar option in MODFLOW-NWT helps convergence). This was added to Version 1.4 of MODFLOW-USG also (but as the default).  
	Included the "SHARED" option in opening files from the NAME-file to allow Parallel runs to access common files. 
	Include "MERGE_BED_K1" option in RIV package to incorporate leakance of river-bed and of GW cell (from river bottom to cell center) in calculations

-------------------------------------------------------------------------------------------------

Fixed bugs/documentation errors for the following packages: 

	Documentation for ICBUND input fixed (when ICBNDFLG is zero it is read)
	BTN Package: Check for input errors of porosity (being less than Sy of transient flow run); 
	BTN Package: Fixed bug if transient flow situation for setting SC2 for confined cases
	GNCN Package: Fixed allocate issue when ISYMGNCn=1 and also for when I2Kn=0 
	WEL Package: fixed bug where NNPWCLN (# of non-parameter CLN wells) was getting reinitialized incorrectly
	SMS Package: bottom dampening feature of NWT was added to Version 1.4 of MODFLOW-USG and is updated here (was added incorrectly)
	CLN Package: Included the "TRANSIENT" option to be backward compatible with MODFLOW-USG v.1.4. The TIB (Transient Ibound) Package can do this also. 
	CLN Package: Included option for "PRINTIAJA" available in MODFLOW-USG v.1.4. 



-------------------------------------------------------------------------------------------------
USG-Transport Version 1.2.0 - Updates to USG-Transport Code and documentation:
----------------------------  -------------------------------------------------------------------

	Added to documentation: Input instructions for transport within CLN. 

	DDF Package: Added option to use vertical thickness weighted averaging for computing z, h, and density at interface instead of just arithmetic averaging. Included documentation for flag ITHICKAV to indicate if arithmetic averating (=0) or vertical thickness weighted averaging would be used in a simulation.

	DDF Package: Included option for implicit update of the head in the density update term. Included documentation for flag IMPDD to indicate explicit or implicit treatment of head in the density term.

Fix saturation initialization for CLN cells in glo2bas1u; 
Fix bug in anisotropic horizontal conductivity formulation (use flux not K in direction of flow)
Fix underrelaxation bug in glo2sms-u1; 
Initialize SC2 in gwf2bcf-lpf-u1; 
Fix budget output in gwf2sfr7u1; 
Fix budget outputs for transport in gwt2bndsu1; 
Fixed bugs in dual porosity transport when dual porosity flow is not run in gwt2dptu1; 
Updated Version number in mfusg
 
Fixed error in dispersion term with updates to code (Glo2btn1u.f) and document. 
Fixed error in WEL, STR, SFR and GHB computation of flux balance for IFMB=1 option in transport. 

--------------------V 1.2.1-4 bug fixes:	
Sorted out CLN and GWF mass flux output for transport - CLN mass flux saved to unit number specified with option SAVECLNMAS (in CLN input file), GWF output on unit IBCTCB (in BCT input file); 
Added FULLYDRY option to Richards equation (Sr is only applied to Kr and not to retention curve); 
Fixed bug in dual porosity flow (was using retention parameters from mobile instead of immobile domain if solving Richards Equation); 
For dual porosity transport with single porosity transient flow needed scaling only for IFRAHK option; 
Fixed CBC boundary flow output for DRT and QRT packages for DRTs and QRTs in GWF and CLN domain; 
Fixed situation where porosity is less than Sy for dual porosity transport with single porosity flow where flow properties are for entire volume (fracture plus matrix)
Fixed initialization error in PCGU
Fixed error in AUX indexing for concentrations

--------------------V 1.3.0 enhancements and bug fixes: 

Fixed FASTFORWARD Option to be flexible (did not work previously if all steps were not saved). 
Added formulation and input instructions for Dual Porosity Flow (DPF) package. 
Added formulation and input instructions for Specified Gradient Boundary (SGB) Package. 
Updated capability in CLN for well efficiency computations is included as an input document update.
Change output TEXT in binary file to identify species for multispecies run (BCT and DPT); 
Fixed LAK package output of budget terms; 
For BCT boundaries, default to zero conc for BCs where AUX is not provided for a species;
Added capability of transport with the LAKE Package (by Dr. Vivek Bedekar at SSPA)

--------------------V 1.4.0 enhancements and bug fixes: 

BCT Package: Included transport of heat as the HEAT option. Updated documentation of BCT to include heat as one of the transport components. 
Included immobile components after the mobile ones in preparation for geochemical reactions. 
Added options for output of binary concentration files to different files instead of just one CON file. 
All these options are documented with the HEAT option package. 
Included ability to add concentration (temperature for thermal component) or mass (heat for thermal component) to the return-flow portion of QRT and DRT packages when solving transport; updated document for QRT and DRT

DRT: Included option for "GHB with return of fixed Q" to help with borehole heat exchanger (BHE) simulations. 

EVT: fixed indexing bugs with zones

TVM v2 Package documentation has been added. THe TVM package was developed by Damian Merrick with solute transport additions by Vivek Bedekar. 

--------------------V 1.5.0 enhancements and bug fixes: 

BCF-LPF: Included tabular input for retention curves for Richards Equation (integrated RE solution needs to be removed). 
Update the "Unsaturated Flow Solution" section of document to include tabular input for Richards equation. 

LAK: Compact budget output of LAKE SEEPAGE did not include all cells due to lake conductivity if-check which was moved to before the II==2 if-check. Flux is zeroed for each lake cell so no residual or undetermined values are written. LAK fixed indexing bug for horizontal connection

CLN Turbulent FLow: Updated document to include details of turbulent flow formulations. 

BCT: Added ITRNSP = 3 option (SS flow w SPs only for transient BCs; BTN fixed dual porosity PCN mass balance component, added time weighting option; added chain decay option; fixed dispersion bug that got formed in V1.3.0; included a MULTIFILE concentration output so each species is written to a separate file instead of all in one file. 

fixed bug in ETS, EVT, RCH transport (to include MCOMPT with temperature input and processing); QRT, DRT fixed initialization bug;  

DPT: Dual porosity transport updated to include heat equation and chain decay. Document updated also. 

WEL: Included WELLBOTTOM  option to read bottom-hole elevation of sink instead of using cell bottom only. 

--------------------V 1.6.0 enhancements and bug fixes: 

BCT: Include ITRNSP=2 option to run transport from previous flow run results. Fixed bug in MULTIFILE option. 
BAS: Added FASTFORWARDC option in BAS to restart transport
CLN: Include documentation of CLN coordinates to assist with placement of CLN cells in GUIs; Include BHE details in CLN cells for heat transport due to conduction through BHE wall, different BHE fluid thermal properties from pore water; and boundary layer within the CLN (convective heat transfer). 
SMS: Added option to SHIFT the RHS and solution vectors before/after linear solve. 

Bug fixes: Fixed MULTIFILE bug for CLN domain , fixed indexing bug in LAKE transport BD; undefined variable IOUTS in MFUSG, Check MXITER of SMS versus restes in OC and stop if OC is larger. Avoid dispersion singularities; add write statement for sequential transport iteration; 

--------------v 1.6.1 fixes bug on MXITER reset in OC package

--------------------V 1.7.0 enhancements and bug fixes: 

QRT: Added transient FHB type hydrogoraph table input for QRT;
FHB: Deallocate variables; include document for FHB for stepped representation of hydrograph
BCT: Solubility limit computations have been added through the option SOLUBILITY. 
BCT: Options for running transport after completion of a flow run (ITRNSP = 2 OR 4) have been revamped from V 1.6.0 where it was implemented. Instead of reading the CBC files, the options now skip the flow solution, read the HDS file, compute all fluid fluxes and mass balances, and enters the BCT packages for transport solution at each time step as needed. 
BCF/LPF: Included alternate compressible storage calculation option (kicks in from zero to full over a small epsilon interval from top instead of changing with grid saturation from zero to full)
DDF: Dual domain flow output of heads bug was fixed (used HNNEW instead of HNEWIM). 
BCT Document: Document for convective heat transfer (BCT Package) shows table calculated using 1 m for characteristic length. The diameter of the BHE tube should be used and the table was updated accordingly. 

--------------------V 1.8.0 enhancements and bug fixes: 

WEL: added WELLBOT option to have different WELLBOT from cell bottom (but not below cell bottom). 
MDT: added matrix diffusion routines (and document) using analytical solutions for matrix diffusion.
BCT: added ITRNSP = 5 option to bypass flow and only run transport using initial concentrations in mobile domain as prescribed concentration boundaries (good for dual porosity or matrix diffusion initialization to load up the matrix with solutes)
SMS: added truncated Newton option (by Damian Merrick).  
Initialization updates in LAK (CNDFC1=0), BAS (TMAXAT default set incorrectly), BTN (MASLOPR = 0; REPLACE status for MULTIFILE option), BCF (IKVFLAG=1 for INLPF NE.0). 
Expanded capability of FASTFORWARD and FASTFORWARDC to start / continue a simulation from an intermediate stress period or time step by reading heads and concentrations from a binary file of a previous run (did not previously work with adaptive time stepping or if binary output was not written at every time step, but does now). 

---------------------


 