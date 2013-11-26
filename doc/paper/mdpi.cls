%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% %%	MDPI class for LaTeX files	08-10-2013
%% %%
%% %%	Class style provided by Stefano Mariani for any information please send an e-mail to:
%% %%		stefano.mariani@polimi.it
%% %%   Modified by:
%% %%		Dietrich Rordorf (rordorf@mdpi.com)
%% %%		Peter Harremoes (entropeter@mdpi.com)
%% %%		Zeno Schumacher (latex@mdpi.com)
%% %%		Maddalena Giulini (latex@mdpi.com)
%% %%		Andres Gartmann (gartmann@mdpi.com) - v1.2
%% %%		Janine Daum (latex@mdpi.com)
%% %%		
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{mdpi}[2013/10/08 MDPI paper class]

\AtEndDocument{\par \cright}

%% To choose the journal
\DeclareOption{actuators}{\gdef\@journal{Actuators}}
\DeclareOption{administrativesciences}{\gdef\@journal{AdministrativeSciences}}
\DeclareOption{aerospace}{\gdef\@journal{Aerospace}}
\DeclareOption{agriculture}{\gdef\@journal{Agriculture}}
\DeclareOption{agronomy}{\gdef\@journal{Agronomy}}
\DeclareOption{algorithms}{\gdef\@journal{Algorithms}}
\DeclareOption{animals}{\gdef\@journal{Animals}}
\DeclareOption{antibiotics}{\gdef\@journal{Antibiotics}}
\DeclareOption{antibodies}{\gdef\@journal{Antibodies}}
\DeclareOption{antioxidants}{\gdef\@journal{Antioxidants}}
\DeclareOption{appliedsciences}{\gdef\@journal{AppliedSciences}}
\DeclareOption{arts}{\gdef\@journal{Arts}}
\DeclareOption{atmosphere}{\gdef\@journal{Atmosphere}}
\DeclareOption{atoms}{\gdef\@journal{Atoms}}
\DeclareOption{axioms}{\gdef\@journal{Axioms}}
\DeclareOption{behavioralsciences}{\gdef\@journal{BehavioralSciences}}
\DeclareOption{bioengineering}{\gdef\@journal{Bioengineering}}
\DeclareOption{biology}{\gdef\@journal{Biology}}
\DeclareOption{biomedicines}{\gdef\@journal{Biomedicines}}
\DeclareOption{biomolecules}{\gdef\@journal{Biomolecules}}
\DeclareOption{biosensors}{\gdef\@journal{Biosensors}}
\DeclareOption{brainsciences}{\gdef\@journal{BrainSciences}}
\DeclareOption{buildings}{\gdef\@journal{Buildings}}
\DeclareOption{cancers}{\gdef\@journal{Cancers}}
\DeclareOption{catalysts}{\gdef\@journal{Catalysts}}
\DeclareOption{cells}{\gdef\@journal{Cells}}
\DeclareOption{challenges}{\gdef\@journal{Challenges}}
\DeclareOption{chemosensors}{\gdef\@journal{Chemosensors}}
\DeclareOption{children}{\gdef\@journal{Children}}
\DeclareOption{chromatography}{\gdef\@journal{Chromatography}}
\DeclareOption{climate}{\gdef\@journal{Climate}}
\DeclareOption{coatings}{\gdef\@journal{Coatings}}
\DeclareOption{computation}{\gdef\@journal{Computation}}
\DeclareOption{computers}{\gdef\@journal{Computers}}
\DeclareOption{cosmetics}{\gdef\@journal{Cosmetics}}
\DeclareOption{crystals}{\gdef\@journal{Crystals}}
\DeclareOption{dentistryjournal}{\gdef\@journal{DentistryJournal}}
\DeclareOption{diagnostics}{\gdef\@journal{Diagnostics}}
\DeclareOption{diseases}{\gdef\@journal{Diseases}}
\DeclareOption{diversity}{\gdef\@journal{Diversity}}
\DeclareOption{econometrics}{\gdef\@journal{Econometrics}}
\DeclareOption{economies}{\gdef\@journal{Economies}}
\DeclareOption{education}{\gdef\@journal{Education}}
\DeclareOption{electronics}{\gdef\@journal{Electronics}}
\DeclareOption{energies}{\gdef\@journal{Energies}}
\DeclareOption{entropy}{\gdef\@journal{Entropy}}
\DeclareOption{environmentalsciences}{\gdef\@journal{EnvironmentalSciences}}
\DeclareOption{fibers}{\gdef\@journal{Fibers}}
\DeclareOption{foods}{\gdef\@journal{Foods}}
\DeclareOption{forests}{\gdef\@journal{Forests}}
\DeclareOption{futureinternet}{\gdef\@journal{FutureInternet}}
\DeclareOption{galaxies}{\gdef\@journal{Galaxies}}
\DeclareOption{games}{\gdef\@journal{Games}}
\DeclareOption{genes}{\gdef\@journal{Genes}}
\DeclareOption{geosciences}{\gdef\@journal{Geosciences}}
\DeclareOption{healthcare}{\gdef\@journal{Healthcare}}
\DeclareOption{humanities}{\gdef\@journal{Humanities}}
\DeclareOption{informatics}{\gdef\@journal{Informatics}}
\DeclareOption{information}{\gdef\@journal{Information}}
\DeclareOption{inorganics}{\gdef\@journal{Inorganics}}
\DeclareOption{insects}{\gdef\@journal{Insects}}
\DeclareOption{ijerph}{\gdef\@journal{Ijerph}}
\DeclareOption{ijfs}{\gdef\@journal{Ijfs}}
\DeclareOption{ijms}{\gdef\@journal{Ijms}}
\DeclareOption{ijgi}{\gdef\@journal{Ijgi}}
\DeclareOption{jcdd}{\gdef\@journal{Jcdd}}
\DeclareOption{jcm}{\gdef\@journal{Jcm}}
\DeclareOption{jdb}{\gdef\@journal{Jdb}}
\DeclareOption{jfb}{\gdef\@journal{Jfb}}
\DeclareOption{joi}{\gdef\@journal{Joi}}
\DeclareOption{jlpea}{\gdef\@journal{Jlpea}}
\DeclareOption{jmse}{\gdef\@journal{Jmse}}
\DeclareOption{jpm}{\gdef\@journal{Jpm}}
\DeclareOption{jrfm}{\gdef\@journal{Jrfm}}
\DeclareOption{jsan}{\gdef\@journal{Jsan}}
\DeclareOption{land}{\gdef\@journal{Land}}
\DeclareOption{laws}{\gdef\@journal{Laws}}
\DeclareOption{life}{\gdef\@journal{Life}}
\DeclareOption{lubricants}{\gdef\@journal{Lubricants}}
\DeclareOption{machines}{\gdef\@journal{Machines}}
\DeclareOption{marinedrugs}{\gdef\@journal{MarineDrugs}}
\DeclareOption{materials}{\gdef\@journal{Materials}}
\DeclareOption{mathematics}{\gdef\@journal{Mathematics}}
\DeclareOption{medicalsciences}{\gdef\@journal{MedicalSciences}}
\DeclareOption{membranes}{\gdef\@journal{Membranes}}
\DeclareOption{metabolites}{\gdef\@journal{Metabolites}}
\DeclareOption{metals}{\gdef\@journal{Metals}}
\DeclareOption{microarrays}{\gdef\@journal{Microarrays}}
\DeclareOption{micromachines}{\gdef\@journal{Micromachines}}
\DeclareOption{microorganisms}{\gdef\@journal{Microorganisms}}
\DeclareOption{minerals}{\gdef\@journal{Minerals}}
\DeclareOption{molbank}{\gdef\@journal{Molbank}}
\DeclareOption{molecules}{\gdef\@journal{Molecules}}
\DeclareOption{nanomaterials}{\gdef\@journal{Nanomaterials}}
\DeclareOption{nutrients}{\gdef\@journal{Nutrients}}
\DeclareOption{optics}{\gdef\@journal{Optics}}
\DeclareOption{pathogens}{\gdef\@journal{Pathogens}}
\DeclareOption{pharmaceuticals}{\gdef\@journal{Pharmaceuticals}}
\DeclareOption{pharmaceutics}{\gdef\@journal{Pharmaceutics}}
\DeclareOption{pharmacy}{\gdef\@journal{Pharmacy}}
\DeclareOption{plants}{\gdef\@journal{Plants}}
\DeclareOption{polymers}{\gdef\@journal{Polymers}}
\DeclareOption{processes}{\gdef\@journal{Processes}}
\DeclareOption{proteomes}{\gdef\@journal{Proteomes}}
\DeclareOption{publications}{\gdef\@journal{Publications}}
\DeclareOption{religions}{\gdef\@journal{Religions}}
\DeclareOption{remotesensing}{\gdef\@journal{RemoteSensing}}
\DeclareOption{resources}{\gdef\@journal{Resources}}
\DeclareOption{risks}{\gdef\@journal{Risks}}
\DeclareOption{robotics}{\gdef\@journal{Robotics}}
\DeclareOption{sensors}{\gdef\@journal{Sensors}}
\DeclareOption{socialsciences}{\gdef\@journal{SocialSciences}}
\DeclareOption{societies}{\gdef\@journal{Societies}}
\DeclareOption{sports}{\gdef\@journal{Sports}}
\DeclareOption{sustainability}{\gdef\@journal{Sustainability}}
\DeclareOption{symmetry}{\gdef\@journal{Symmetry}}
\DeclareOption{systems}{\gdef\@journal{Systems}}
\DeclareOption{technologies}{\gdef\@journal{Technologies}}
\DeclareOption{toxics}{\gdef\@journal{Toxics}}
\DeclareOption{toxins}{\gdef\@journal{Toxins}}
\DeclareOption{vaccines}{\gdef\@journal{Vaccines}}
\DeclareOption{veterinarysciences}{\gdef\@journal{VeterinarySciences}}
\DeclareOption{viruses}{\gdef\@journal{Viruses}}
\DeclareOption{water}{\gdef\@journal{Water}}

%% Uppercase will have the same effect
\DeclareOption{Actuators}{\gdef\@journal{Actuators}}
\DeclareOption{Administrativesciences}{\gdef\@journal{AdministrativeSciences}}
\DeclareOption{Aerospace}{\gdef\@journal{Aerospace}}
\DeclareOption{Agriculture}{\gdef\@journal{Agriculture}}
\DeclareOption{Agronomy}{\gdef\@journal{Agronomy}}
\DeclareOption{Algorithms}{\gdef\@journal{Algorithms}}
\DeclareOption{Animals}{\gdef\@journal{Animals}}
\DeclareOption{Antibiotics}{\gdef\@journal{Antibiotics}}
\DeclareOption{Antibodies}{\gdef\@journal{Antibodies}}
\DeclareOption{Antioxidants}{\gdef\@journal{Antioxidants}}
\DeclareOption{Appliedsciences}{\gdef\@journal{AppliedSciences}}
\DeclareOption{Arts}{\gdef\@journal{Arts}}
\DeclareOption{Atmosphere}{\gdef\@journal{Atmosphere}}
\DeclareOption{Atoms}{\gdef\@journal{Atoms}}
\DeclareOption{Axioms}{\gdef\@journal{Axioms}}
\DeclareOption{Behavioralsciences}{\gdef\@journal{BehavioralSciences}}
\DeclareOption{Bioengineering}{\gdef\@journal{Bioengineering}}
\DeclareOption{Biology}{\gdef\@journal{Biology}}
\DeclareOption{Biomedicines}{\gdef\@journal{Biomedicines}}
\DeclareOption{Biomolecules}{\gdef\@journal{Biomolecules}}
\DeclareOption{Biosensors}{\gdef\@journal{Biosensors}}
\DeclareOption{Brainsciences}{\gdef\@journal{BrainSciences}}
\DeclareOption{Buildings}{\gdef\@journal{Buildings}}
\DeclareOption{Cancers}{\gdef\@journal{Cancers}}
\DeclareOption{Catalysts}{\gdef\@journal{Catalysts}}
\DeclareOption{Cells}{\gdef\@journal{Cells}}
\DeclareOption{Challenges}{\gdef\@journal{Challenges}}
\DeclareOption{Chemosensors}{\gdef\@journal{Chemosensors}}
\DeclareOption{Children}{\gdef\@journal{Children}}
\DeclareOption{Chromatography}{\gdef\@journal{Chromatography}}
\DeclareOption{Climate}{\gdef\@journal{Climate}}
\DeclareOption{Coatings}{\gdef\@journal{Coatings}}
\DeclareOption{Computation}{\gdef\@journal{Computation}}
\DeclareOption{Computers}{\gdef\@journal{Computers}}
\DeclareOption{Cosmetics}{\gdef\@journal{Cosmetics}}
\DeclareOption{Crystals}{\gdef\@journal{Crystals}}
\DeclareOption{Dentistryjournal}{\gdef\@journal{DentistryJournal}}
\DeclareOption{Diagnostics}{\gdef\@journal{Diagnostics}}
\DeclareOption{Diseases}{\gdef\@journal{Diseases}}
\DeclareOption{Diversity}{\gdef\@journal{Diversity}}
\DeclareOption{Econometrics}{\gdef\@journal{Econometrics}}
\DeclareOption{Economies}{\gdef\@journal{Economies}}
\DeclareOption{Education}{\gdef\@journal{Education}}
\DeclareOption{Electronics}{\gdef\@journal{Electronics}}
\DeclareOption{Energies}{\gdef\@journal{Energies}}
\DeclareOption{Entropy}{\gdef\@journal{Entropy}}
\DeclareOption{Environmentalsciences}{\gdef\@journal{EnvironmentalSciences}}
\DeclareOption{Fibers}{\gdef\@journal{Fibers}}
\DeclareOption{Foods}{\gdef\@journal{Foods}}
\DeclareOption{Forests}{\gdef\@journal{Forests}}
\DeclareOption{Futureinternet}{\gdef\@journal{FutureInternet}}
\DeclareOption{Galaxies}{\gdef\@journal{Galaxies}}
\DeclareOption{Games}{\gdef\@journal{Games}}
\DeclareOption{Genes}{\gdef\@journal{Genes}}
\DeclareOption{Geosciences}{\gdef\@journal{Geosciences}}
\DeclareOption{Healthcare}{\gdef\@journal{Healthcare}}
\DeclareOption{Humanities}{\gdef\@journal{Humanities}}
\DeclareOption{Informatics}{\gdef\@journal{Informatics}}
\DeclareOption{Information}{\gdef\@journal{Information}}
\DeclareOption{Inorganics}{\gdef\@journal{Inorganics}}
\DeclareOption{Insects}{\gdef\@journal{Insects}}
\DeclareOption{Ijerph}{\gdef\@journal{Ijerph}}
\DeclareOption{Ijfs}{\gdef\@journal{Ijfs}}
\DeclareOption{Ijms}{\gdef\@journal{Ijms}}
\DeclareOption{Ijgi}{\gdef\@journal{Ijgi}}
\DeclareOption{Jcdd}{\gdef\@journal{Jcdd}}
\DeclareOption{Jcm}{\gdef\@journal{Jcm}}
\DeclareOption{Jdb}{\gdef\@journal{Jdb}}
\DeclareOption{Jfb}{\gdef\@journal{Jfb}}
\DeclareOption{Joi}{\gdef\@journal{Joi}}
\DeclareOption{Jlpea}{\gdef\@journal{Jlpea}}
\DeclareOption{Jmse}{\gdef\@journal{Jmse}}
\DeclareOption{Jpm}{\gdef\@journal{Jpm}}
\DeclareOption{Jrfm}{\gdef\@journal{Jrfm}}
\DeclareOption{Jsan}{\gdef\@journal{Jsan}}
\DeclareOption{Land}{\gdef\@journal{Land}}
\DeclareOption{Laws}{\gdef\@journal{Laws}}
\DeclareOption{Life}{\gdef\@journal{Life}}
\DeclareOption{Lubricants}{\gdef\@journal{Lubricants}}
\DeclareOption{Machines}{\gdef\@journal{Machines}}
\DeclareOption{MarineDrugs}{\gdef\@journal{MarineDrugs}}
\DeclareOption{Materials}{\gdef\@journal{Materials}}
\DeclareOption{Mathematics}{\gdef\@journal{Mathematics}}
\DeclareOption{Medicalsciences}{\gdef\@journal{MedicalSciences}}
\DeclareOption{Membranes}{\gdef\@journal{Membranes}}
\DeclareOption{Metabolites}{\gdef\@journal{Metabolites}}
\DeclareOption{Metals}{\gdef\@journal{Metals}}
\DeclareOption{Microarrays}{\gdef\@journal{Microarrays}}
\DeclareOption{Micromachines}{\gdef\@journal{Micromachines}}
\DeclareOption{Microorganisms}{\gdef\@journal{Microorganisms}}
\DeclareOption{Minerals}{\gdef\@journal{Minerals}}
\DeclareOption{Molbank}{\gdef\@journal{Molbank}}
\DeclareOption{Molecules}{\gdef\@journal{Molecules}}
\DeclareOption{Nanomaterials}{\gdef\@journal{Nanomaterials}}
\DeclareOption{Nutrients}{\gdef\@journal{Nutrients}}
\DeclareOption{Optics}{\gdef\@journal{Optics}}
\DeclareOption{Pathogens}{\gdef\@journal{Pathogens}}
\DeclareOption{Pharmaceuticals}{\gdef\@journal{Pharmaceuticals}}
\DeclareOption{Pharmaceutics}{\gdef\@journal{Pharmaceutics}}
\DeclareOption{Pharmacy}{\gdef\@journal{Pharmacy}}
\DeclareOption{Plants}{\gdef\@journal{Plants}}
\DeclareOption{Polymers}{\gdef\@journal{Polymers}}
\DeclareOption{Processes}{\gdef\@journal{Processes}}
\DeclareOption{Proteomes}{\gdef\@journal{Proteomes}}
\DeclareOption{Publications}{\gdef\@journal{Publications}}
\DeclareOption{Religions}{\gdef\@journal{Religions}}
\DeclareOption{RemoteSensing}{\gdef\@journal{RemoteSensing}}
\DeclareOption{Resources}{\gdef\@journal{Resources}}
\DeclareOption{Risks}{\gdef\@journal{Risks}}
\DeclareOption{Robotics}{\gdef\@journal{Robotics}}
\DeclareOption{Sensors}{\gdef\@journal{Sensors}}
\DeclareOption{SocialSciences}{\gdef\@journal{SocialSciences}}
\DeclareOption{Societies}{\gdef\@journal{Societies}}
\DeclareOption{Sports}{\gdef\@journal{Sports}}
\DeclareOption{Sustainability}{\gdef\@journal{Sustainability}}
\DeclareOption{Symmetry}{\gdef\@journal{Symmetry}}
\DeclareOption{Systems}{\gdef\@journal{Systems}}
\DeclareOption{Technologies}{\gdef\@journal{Technologies}}
\DeclareOption{Toxics}{\gdef\@journal{Toxics}}
\DeclareOption{Toxins}{\gdef\@journal{Toxins}}
\DeclareOption{Vaccines}{\gdef\@journal{Vaccines}}
\DeclareOption{VeterinarySciences}{\gdef\@journal{VeterinarySciences}}
\DeclareOption{Viruses}{\gdef\@journal{Viruses}}
\DeclareOption{Water}{\gdef\@journal{Water}}
\DeclareOption{journal}{\ClassWarning{mdpi}{You have not specified the journal. The first class option of the document class mdpi specify the journal. The word journal should be replaced by one of the following journal names: actuators, administrativesciences, aerospace, agriculture, agronomy, algorithms, animals, antibiotics, antibodies, antioxidants, appliedsciences, arts, atmosphere, atoms, axioms, behavioralsciences, bioengineering, biology, biomedicines, biomolecules, biosensors, brainsciences, buildings, cancers, catalysts, cells, challenges, chemosensors, children, chromatography, climate, coatings, computation, computers, cosmetics, crystals, dentistryjournal, diagnostics, diseases, diversity, econometrics, economies, education, electronics, energies, entropy, environmentalsciences, fibers, foods, forests, futureinternet, galaxies, games, genes, geosciences, healthcare, humanities, informatics, information, inorganics, insects, ijerph, ijfs, ijms, ijgi, jcdd, jcm, jdb, jfb, joi, jlpea, jmse, jpm, jrfm, jsan, land, laws, life, lubricants, machines, marinedrugs, materials, mathematics, medicalsciences, membranes, metabolites, metals, microarrays, micromachines, microorganisms, minerals, molbank, molecules, nanomaterials, nutrients, optics, pathogens, pharmaceuticals, pharmaceutics, pharmacy, plants, polymers, processes, proteomes, publications, religions, remotesensing, resources, risks, robotics, sensors, socialsciences, societies, sports, sustainability, symmetry, systems, technologies, toxics, toxins, vaccines, veterinarysciences, viruses, water}} 

%% To choose the type of manuscript
\DeclareOption{article}{\gdef\@arttype{Article}}
\DeclareOption{review}{\gdef\@arttype{Review}}
\DeclareOption{communication}{\gdef\@arttype{Communication}}
\DeclareOption{commentary}{\gdef\@arttype{Commentary}}
\DeclareOption{bookreview}{\gdef\@arttype{Book Review}}
\DeclareOption{correction}{\gdef\@arttype{Correction}}
\DeclareOption{addendum}{\gdef\@arttype{Addendum}}
\DeclareOption{editorial}{\gdef\@arttype{Editorial}}
\DeclareOption{changes}{\gdef\@arttype{Changes}}
\DeclareOption{supfile}{\gdef\@arttype{Supfile}}

%% To choose the status of the manuscript
\DeclareOption{submit}{\gdef\@status{submit}}
\DeclareOption{accept}{\gdef\@status{accept}}

%% To choose the whether there is one or more authors
\DeclareOption{oneauthor}{\gdef\@authornum{author}}
\DeclareOption{moreauthors}{\gdef\@authornum{authors}}
\DeclareOption{10pt}{\OptionNotUsed}
\DeclareOption{twocolumn}{\OptionNotUsed}
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\ExecuteOptions{entropy,12pt,a4paper,article,submit,oneauthor}
\ProcessOptions\relax

\LoadClass[12pt]{article}

\RequirePackage{calc}
\RequirePackage{indentfirst}
\RequirePackage{times,fancyhdr}
\RequirePackage{graphicx}
\RequirePackage{lastpage}
\RequirePackage{ifthen}
\RequirePackage{lineno}
\RequirePackage{float}
\RequirePackage{amsmath}
\floatstyle{plaintop}
\restylefloat{figure}
\restylefloat{table}
\usepackage{caption}
\captionsetup{position=top, margin=1cm, labelfont=bf, labelsep=period, skip=12pt, justification=justified}
\RequirePackage{color}
\ifthenelse{\equal{\@journal}{Actuators}}{\definecolor{logocolor}{rgb}{0.02,0.37,0.035}}{}%
\ifthenelse{\equal{\@journal}{AdministrativeSciences}}{\definecolor{logocolor}{rgb}{0.31,0.51,0.60}}{}%
\ifthenelse{\equal{\@journal}{Aerospace}}{\definecolor{logocolor}{rgb}{0.0,0.126,0.375}}{}%
\ifthenelse{\equal{\@journal}{Agriculture}}{\definecolor{logocolor}{rgb}{0.23,0.42,0.18}}{}%
\ifthenelse{\equal{\@journal}{Agronomy}}{\definecolor{logocolor}{rgb}{0.21,0.31,0.09}}{}%
\ifthenelse{\equal{\@journal}{Algorithms}}{\definecolor{logocolor}{rgb}{0.31,0.46,0}}{}%
\ifthenelse{\equal{\@journal}{Animals}}{\definecolor{logocolor}{rgb}{0.05,0.26,0.44}}{}%
\ifthenelse{\equal{\@journal}{Antibiotics}}{\definecolor{logocolor}{rgb}{0.332,0,0}}{}%
\ifthenelse{\equal{\@journal}{Antibodies}}{\definecolor{logocolor}{rgb}{0.4,0.0,0.4}}{}%
\ifthenelse{\equal{\@journal}{Antioxidants}}{\definecolor{logocolor}{rgb}{0.278,0.129,0.573}}{}%
\ifthenelse{\equal{\@journal}{AppliedSciences}}{\definecolor{logocolor}{rgb}{0.29,0.29,0.5}}{}%
\ifthenelse{\equal{\@journal}{Arts}}{\definecolor{logocolor}{rgb}{0.066,0.457,0.711}}{}%
\ifthenelse{\equal{\@journal}{Atmosphere}}{\definecolor{logocolor}{rgb}{0.2,0.2,0.6}}{}%
\ifthenelse{\equal{\@journal}{Atoms}}{\definecolor{logocolor}{rgb}{0.0,0.13,0.17}}{}%
\ifthenelse{\equal{\@journal}{Axioms}}{\definecolor{logocolor}{rgb}{0.51,0.56,0.21}}{}%
\ifthenelse{\equal{\@journal}{BehavioralSciences}}{\definecolor{logocolor}{rgb}{0,0.31,0.44}}{}%
\ifthenelse{\equal{\@journal}{Bioengineering}}{\definecolor{logocolor}{rgb}{0.0,0.434,0.07}}{}%
\ifthenelse{\equal{\@journal}{Biology}}{\definecolor{logocolor}{rgb}{0.16,0.19,0.45}}{}%
\ifthenelse{\equal{\@journal}{Biomedicines}}{\definecolor{logocolor}{rgb}{0.078,0.25,0.60}}{}%
\ifthenelse{\equal{\@journal}{Biomolecules}}{\definecolor{logocolor}{rgb}{0.22,0.44,0.13}}{}%
\ifthenelse{\equal{\@journal}{Biosensors}}{\definecolor{logocolor}{rgb}{0.16,0.37,0.27}}{}%
\ifthenelse{\equal{\@journal}{BrainSciences}}{\definecolor{logocolor}{rgb}{0,0.36,0.44}}{}%
\ifthenelse{\equal{\@journal}{Buildings}}{\definecolor{logocolor}{rgb}{0.4,0.4,0.2}}{}%
\ifthenelse{\equal{\@journal}{Cancers}}{\definecolor{logocolor}{rgb}{0,0.20,0.60}}{}%
\ifthenelse{\equal{\@journal}{Catalysts}}{\definecolor{logocolor}{rgb}{0.5,0.24,0.03}}{}%
\ifthenelse{\equal{\@journal}{Cells}}{\definecolor{logocolor}{rgb}{0.09,0.33,0.75}}{}%
\ifthenelse{\equal{\@journal}{Challenges}}{\definecolor{logocolor}{rgb}{0.13,0.44,0.23}}{}%
\ifthenelse{\equal{\@journal}{Chemosensors}}{\definecolor{logocolor}{rgb}{0.0,0.498,0.498}}{}%
\ifthenelse{\equal{\@journal}{Children}}{\definecolor{logocolor}{rgb}{0.114,0.494,0.631}}{}%
\ifthenelse{\equal{\@journal}{Chromatography}}{\definecolor{logocolor}{rgb}{0.337,0.267,0.267}}{}%
\ifthenelse{\equal{\@journal}{Climate}}{\definecolor{logocolor}{rgb}{0.165,0.298,0.714}}{}%
\ifthenelse{\equal{\@journal}{Coatings}}{\definecolor{logocolor}{rgb}{0.25,0.16,0.02}}{}%
\ifthenelse{\equal{\@journal}{Computation}}{\definecolor{logocolor}{rgb}{0.392,0.4,0.180}}{}%
\ifthenelse{\equal{\@journal}{Computers}}{\definecolor{logocolor}{rgb}{0.4,0.4,0}}{}%
\ifthenelse{\equal{\@journal}{Cosmetics}}{\definecolor{logocolor}{rgb}{0.153,0.627,0.49}}{}%
\ifthenelse{\equal{\@journal}{Crystals}}{\definecolor{logocolor}{rgb}{0.21,0.43,0.69}}{}%
\ifthenelse{\equal{\@journal}{DentistryJournal}}{\definecolor{logocolor}{rgb}{0.664, 0.266,0}}{}%
\ifthenelse{\equal{\@journal}{Diagnostics}}{\definecolor{logocolor}{rgb}{0.17,0.51,0.56}}{}%
\ifthenelse{\equal{\@journal}{Diseases}}{\definecolor{logocolor}{rgb}{0.24,0.24,0.24}}{}%
\ifthenelse{\equal{\@journal}{Diversity}}{\definecolor{logocolor}{rgb}{0.25,0.41,0.49}}{}%
\ifthenelse{\equal{\@journal}{Econometrics}}{\definecolor{logocolor}{rgb}{0.46,0.35,0.05}}{}%
\ifthenelse{\equal{\@journal}{Economies}}{\definecolor{logocolor}{rgb}{0.29,0.51,0.44}}{}%
\ifthenelse{\equal{\@journal}{Education}}{\definecolor{logocolor}{rgb}{0.055,0.434,0.051}}{}%
\ifthenelse{\equal{\@journal}{Electronics}}{\definecolor{logocolor}{rgb}{0.35,0.44,0.0}}{}%
\ifthenelse{\equal{\@journal}{Energies}}{\definecolor{logocolor}{rgb}{0.6,0.2,0}}{}%
\ifthenelse{\equal{\@journal}{Entropy}}{\definecolor{logocolor}{rgb}{0,0.502,0.502}}{}%
\ifthenelse{\equal{\@journal}{EnvironmentalSciences}}{\definecolor{logocolor}{rgb}{0.447,0.557,0.180}}{}%
\ifthenelse{\equal{\@journal}{Fibers}}{\definecolor{logocolor}{rgb}{0.235,0.067,0.267}}{}%
\ifthenelse{\equal{\@journal}{Foods}}{\definecolor{logocolor}{rgb}{0.34,0.53,0.08}}{}%
\ifthenelse{\equal{\@journal}{Forests}}{\definecolor{logocolor}{rgb}{0.16,0.31,0.13}}{}%
\ifthenelse{\equal{\@journal}{FutureInternet}}{\definecolor{logocolor}{rgb}{0.28,0.32,0.43}}{}%
\ifthenelse{\equal{\@journal}{Galaxies}}{\definecolor{logocolor}{rgb}{0.10,0,0.62}}{}%
\ifthenelse{\equal{\@journal}{Games}}{\definecolor{logocolor}{rgb}{0.34,0.18,0.14}}{}%
\ifthenelse{\equal{\@journal}{Genes}}{\definecolor{logocolor}{rgb}{0.37,0.29,0.48}}{}%
\ifthenelse{\equal{\@journal}{Geosciences}}{\definecolor{logocolor}{rgb}{0.0,0.2,0.6}}{}%
\ifthenelse{\equal{\@journal}{Healthcare}}{\definecolor{logocolor}{rgb}{0.398,0.027,0.441}}{}%
\ifthenelse{\equal{\@journal}{Humanities}}{\definecolor{logocolor}{rgb}{0.27,0.4,0.86}}{}%
\ifthenelse{\equal{\@journal}{Informatics}}{\definecolor{logocolor}{rgb}{0.07,0,0.75}}{}%
\ifthenelse{\equal{\@journal}{Information}}{\definecolor{logocolor}{rgb}{0.03,0.5,0.27}}{}%
\ifthenelse{\equal{\@journal}{Inorganics}}{\definecolor{logocolor}{rgb}{0.42,0.188,0.282}}{}%
\ifthenelse{\equal{\@journal}{Insects}}{\definecolor{logocolor}{rgb}{0.18,0.37,0.11}}{}%
\ifthenelse{\equal{\@journal}{Ijerph}}{\definecolor{logocolor}{rgb}{0.26,0.43,0.13}}{}%
\ifthenelse{\equal{\@journal}{Ijfs}}{\definecolor{logocolor}{rgb}{0.33,0.27,0}}{}%
\ifthenelse{\equal{\@journal}{Ijms}}{\definecolor{logocolor}{rgb}{0, 0.5, 0.5}}{}%
\ifthenelse{\equal{\@journal}{Ijgi}}{\definecolor{logocolor}{rgb}{0.01,0.26,0.55}}{}%
\ifthenelse{\equal{\@journal}{Jcdd}}{\definecolor{logocolor}{rgb}{0.398,0.0430,0.383}}{}%
\ifthenelse{\equal{\@journal}{Jcm}}{\definecolor{logocolor}{rgb}{0.07,0.37,0.34}}{}%
\ifthenelse{\equal{\@journal}{Jdb}}{\definecolor{logocolor}{rgb}{0.28,0.38,0.53}}{}%
\ifthenelse{\equal{\@journal}{Jfb}}{\definecolor{logocolor}{rgb}{0.07,0.37,0.34}}{}%
\ifthenelse{\equal{\@journal}{Joi}}{\definecolor{logocolor}{rgb}{0.31,0.29,0.50}}{}%
\ifthenelse{\equal{\@journal}{Jlpea}}{\definecolor{logocolor}{rgb}{0.07,0.37,0.22}}{}%
\ifthenelse{\equal{\@journal}{Jmse}}{\definecolor{logocolor}{rgb}{0.059,0.059,0.431}}{}%
\ifthenelse{\equal{\@journal}{Jpm}}{\definecolor{logocolor}{rgb}{0,0.40,0.32}}{}%
\ifthenelse{\equal{\@journal}{Jrfm}}{\definecolor{logocolor}{rgb}{0,0.238,0.434}}{}%
\ifthenelse{\equal{\@journal}{Jsan}}{\definecolor{logocolor}{rgb}{0.04,0.16,0.1}}{}%
\ifthenelse{\equal{\@journal}{Land}}{\definecolor{logocolor}{rgb}{0.373,0.247,0.188}}{}%
\ifthenelse{\equal{\@journal}{Laws}}{\definecolor{logocolor}{rgb}{0.32,0.12,0.38}}{}%
\ifthenelse{\equal{\@journal}{Life}}{\definecolor{logocolor}{rgb}{0.13,0.46,0.85}}{}%
\ifthenelse{\equal{\@journal}{Lubricants}}{\definecolor{logocolor}{rgb}{0.23,0.20,0.18}}{}%
\ifthenelse{\equal{\@journal}{Machines}}{\definecolor{logocolor}{rgb}{0.05,0.01,0.45}}{}%
\ifthenelse{\equal{\@journal}{MarineDrugs}}{\definecolor{logocolor}{rgb}{0,0.502,0.502}}{}%
\ifthenelse{\equal{\@journal}{Materials}}{\definecolor{logocolor}{rgb}{0.6,0.4,0.2}}{}%
\ifthenelse{\equal{\@journal}{Mathematics}}{\definecolor{logocolor}{rgb}{0.227,0.161,0.090}}{}%
\ifthenelse{\equal{\@journal}{MedicalSciences}}{\definecolor{logocolor}{rgb}{0.180,0.360,0.391}}{}%
\ifthenelse{\equal{\@journal}{Membranes}}{\definecolor{logocolor}{rgb}{0.5,0.2,0.03}}{}%
\ifthenelse{\equal{\@journal}{Metabolites}}{\definecolor{logocolor}{rgb}{0.14,0.31,0.34}}{}%
\ifthenelse{\equal{\@journal}{Metals}}{\definecolor{logocolor}{rgb}{0.56,0.3,0}}{}%
\ifthenelse{\equal{\@journal}{Microarrays}}{\definecolor{logocolor}{rgb}{0.45,0.56,0.63}}{}%
\ifthenelse{\equal{\@journal}{Micromachines}}{\definecolor{logocolor}{rgb}{0,0.2,0.6}}{}%
\ifthenelse{\equal{\@journal}{Microorganisms}}{\definecolor{logocolor}{rgb}{0.008,0.031,0.255}}{}%
\ifthenelse{\equal{\@journal}{Molecules}}{\definecolor{logocolor}{rgb}{0,0.502,0.502}}{}%
\ifthenelse{\equal{\@journal}{Minerals}}{\definecolor{logocolor}{rgb}{0.54,0.24,0.01}}{}%
\ifthenelse{\equal{\@journal}{Molbank}}{\definecolor{logocolor}{rgb}{0,0.502,0.502}}{}%
\ifthenelse{\equal{\@journal}{Nanomaterials}}{\definecolor{logocolor}{rgb}{0.26,0.31,0.69}}{}%
\ifthenelse{\equal{\@journal}{Nutrients}}{\definecolor{logocolor}{rgb}{0.20,0.40,0.20}}{}%
\ifthenelse{\equal{\@journal}{Optics}}{\definecolor{logocolor}{rgb}{0.208,0.208,0.337}}{}%
\ifthenelse{\equal{\@journal}{Pathogens}}{\definecolor{logocolor}{rgb}{0.6,0.2,0.4}}{}%
\ifthenelse{\equal{\@journal}{Pharmaceuticals}}{\definecolor{logocolor}{rgb}{0.44,0.19,0.63}}{}%
\ifthenelse{\equal{\@journal}{Pharmaceutics}}{\definecolor{logocolor}{rgb}{0.20,0.40,0.40}}{}%
\ifthenelse{\equal{\@journal}{Pharmacy}}{\definecolor{logocolor}{rgb}{0.40,0.16,0.48}}{}%
\ifthenelse{\equal{\@journal}{Plants}}{\definecolor{logocolor}{rgb}{0.27,0.46,0.0}}{}%
\ifthenelse{\equal{\@journal}{Polymers}}{\definecolor{logocolor}{rgb}{0.44,0.19,0.63}}{}%
\ifthenelse{\equal{\@journal}{Processes}}{\definecolor{logocolor}{rgb}{0.678,0.408,0.376}}{}%
\ifthenelse{\equal{\@journal}{Proteomes}}{\definecolor{logocolor}{rgb}{0.07,0.38,0.15}}{}%
\ifthenelse{\equal{\@journal}{Publications}}{\definecolor{logocolor}{rgb}{0.259,0.216,0.0353}}{}%
\ifthenelse{\equal{\@journal}{Religions}}{\definecolor{logocolor}{rgb}{0,0.4,0.6}}{}%
\ifthenelse{\equal{\@journal}{RemoteSensing}}{\definecolor{logocolor}{rgb}{0.20,0.40,0}}{}%
\ifthenelse{\equal{\@journal}{Resources}}{\definecolor{logocolor}{rgb}{0.06,0.33,0.49}}{}%
\ifthenelse{\equal{\@journal}{Risks}}{\definecolor{logocolor}{rgb}{0.62,0,0.31}}{}%
\ifthenelse{\equal{\@journal}{Robotics}}{\definecolor{logocolor}{rgb}{0.239,0.263,0.471}}{}%
\ifthenelse{\equal{\@journal}{Sensors}}{\definecolor{logocolor}{rgb}{0,0.502,0.502}}{}%
\ifthenelse{\equal{\@journal}{SocialSciences}}{\definecolor{logocolor}{rgb}{0.15,0.29,0.4}}{}%
\ifthenelse{\equal{\@journal}{Societies}}{\definecolor{logocolor}{rgb}{0.08,0.43,0.62}}{}%
\ifthenelse{\equal{\@journal}{Sports}}{\definecolor{logocolor}{rgb}{0.42,0.24,0.18}}{}%
\ifthenelse{\equal{\@journal}{Sustainability}}{\definecolor{logocolor}{rgb}{0.21,0.37,0.57}}{}%
\ifthenelse{\equal{\@journal}{Symmetry}}{\definecolor{logocolor}{rgb}{0,0.20,0.20}}{}%
\ifthenelse{\equal{\@journal}{Systems}}{\definecolor{logocolor}{rgb}{0.133,0.333,0.0}}{}%
\ifthenelse{\equal{\@journal}{Technologies}}{\definecolor{logocolor}{rgb}{0.055,0.114,0.286}}{}%
\ifthenelse{\equal{\@journal}{Toxics}}{\definecolor{logocolor}{rgb}{0.582, 0.082,0.684}}{}%
\ifthenelse{\equal{\@journal}{Toxins}}{\definecolor{logocolor}{rgb}{0.60,0.20,0}}{}%
\ifthenelse{\equal{\@journal}{Vaccines}}{\definecolor{logocolor}{rgb}{0.2,0.2,0.2}}{}%
\ifthenelse{\equal{\@journal}{VeterinarySciences}}{\definecolor{logocolor}{rgb}{0.441,0,0.383}}{}%
\ifthenelse{\equal{\@journal}{Viruses}}{\definecolor{logocolor}{rgb}{0.5,0,0}}{}%
\ifthenelse{\equal{\@journal}{Water}}{\definecolor{logocolor}{rgb}{0.21,0.37,0.57}}{}%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\RequirePackage[sort&compress]{natbib}
\bibpunct{[}{]}{,}{n}{}{}
\RequirePackage{hyphenat}
\hyphenpenalty=2000\exhyphenpenalty=2000\sloppy

\renewcommand\NAT@set@cites{%
  \ifNAT@numbers
    \ifNAT@super \let\@cite\NAT@citesuper
       \def\NAT@mbox##1{\unskip\nobreak\textsuperscript{##1}}%
       \let\citeyearpar=\citeyear
       \let\NAT@space\relax
       \def\NAT@super@kern{\kern\p@}%
    \else
       \let\NAT@mbox=\mbox
       \let\@cite\NAT@citenum
       \let\NAT@space\relax
       \let\NAT@super@kern\relax
    \fi
    \let\@citex\NAT@citexnum
    \let\@biblabel\NAT@biblabelnum
    \let\@bibsetup\NAT@bibsetnum
    \renewcommand\NAT@idxtxt{\NAT@name\NAT@spacechar\NAT@open\NAT@num\NAT@close}%
    \def\natexlab##1{}%
    \def\NAT@penalty{\penalty\@m}%
  \else
    \let\@cite\NAT@cite
    \let\@citex\NAT@citex
    \let\@biblabel\NAT@biblabel
    \let\@bibsetup\NAT@bibsetup
    \let\NAT@space\NAT@spacechar
    \let\NAT@penalty\@empty
    \renewcommand\NAT@idxtxt{\NAT@name\NAT@spacechar\NAT@open\NAT@date\NAT@close}%
    \def\natexlab##1{##1}%
  \fi}

\renewcommand{\baselinestretch}{1.19}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\RequirePackage[unicode=false, pdftoolbar=true, pdfmenubar=true, bookmarksopen={true},
pdffitwindow=true, pdfnewwindow=true, colorlinks=true,
linkcolor=blue, citecolor=blue, urlcolor=blue, hyperfootnotes=false]{hyperref}
\newcommand{\org@maketitle}{}% LATEX-Check
\let\org@maketitle\maketitle
\def\maketitle{%
\hypersetup{
pdftitle={\@Title},
pdfsubject={\@abstract},
pdfkeywords={\@keyword}
}%
\org@maketitle
}
\renewcommand{\@pdfauthor}{\@Author}

%% Footnotes (corresponding to the MDPI Word template)
\RequirePackage[hang]{footmisc}
\setlength{\skip\footins}{1.2cm}
\setlength{\footnotemargin}{5mm}
\def\footnoterule{\kern-14\p@
\hrule \@width 2in \kern 11.6\p@}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\RequirePackage{url}
\urlstyle{same}
\clubpenalty=10000
\widowpenalty=10000
\displaywidowpenalty=10000

%%%%For tracking changes
%\ifthenelse{\equal{\@status}{accept}}{\newcommand{\cor}[2]{{#2}}}{
%\newcommand{\cor}[2]{{\textcolor{red}{\st{#1 }}}\textcolor[rgb]{0.25,0,0.5}{#2}}}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\newcommand{\firstargument}{}
\newcommand{\Title}[1]{\gdef\@Title{#1}}%
\newcommand{\Author}[1]{\gdef\@Author{#1}}%
\newcommand{\lastpage}[1]{\gdef\@lastpage{#1}}
\newcommand{\doinum}[1]{\gdef\@doinum{#1}}
\newcommand{\pubvolume}[1]{\gdef\@pubvolume{#1}}
\newcommand{\pubyear}[1]{\gdef\@pubyear{#1}}
\newcommand{\address}[2][]{\renewcommand{\firstargument}{#1}\gdef\@address{#2}}
\newcommand{\corresfirstargument}{}
\newcommand{\corres}[2][]{\renewcommand{\corresfirstargument}{#1}\gdef\@corres{#2}}
\renewcommand{\abstract}[1]{\gdef\@abstract{#1}}
\newcommand{\history}[1]{\gdef\@history{#1}} \def\@pacs{}
\newcommand{\PACS}[1]{\gdef\@pacs{#1}} \def\@msc{}
\newcommand{\MSC}[1]{\gdef\@msc{#1}}
\newcommand{\keyword}[1]{\gdef\@keyword{#1}}

%% Define the journal name for the header using the ifthen package
\newcommand{\journalname}{%
  \ifthenelse{\equal{\@journal}{Actuators}}{Actuators}{}%
  \ifthenelse{\equal{\@journal}{AdministrativeSciences}}{Adm. Sci.}{}%
  \ifthenelse{\equal{\@journal}{Aerospace}}{Aerospace}{}%
  \ifthenelse{\equal{\@journal}{Agriculture}}{Agriculture}{}%
  \ifthenelse{\equal{\@journal}{Agronomy}}{Agronomy}{}%
  \ifthenelse{\equal{\@journal}{Algorithms}}{Algorithms}{}%
  \ifthenelse{\equal{\@journal}{Animals}}{Animals}{}%
  \ifthenelse{\equal{\@journal}{Antibiotics}}{Antibiotics}{}%
  \ifthenelse{\equal{\@journal}{Antibodies}}{Antibodies}{}%
  \ifthenelse{\equal{\@journal}{Antioxidants}}{Antioxidants}{}%
  \ifthenelse{\equal{\@journal}{AppliedSciences}}{Appl. Sci.}{}%
  \ifthenelse{\equal{\@journal}{Arts}}{Arts}{}%
  \ifthenelse{\equal{\@journal}{Atmosphere}}{Atmosphere}{}%
  \ifthenelse{\equal{\@journal}{Atoms}}{Atoms}{}%
  \ifthenelse{\equal{\@journal}{Axioms}}{Axioms}{}%
  \ifthenelse{\equal{\@journal}{BehavioralSciences}}{Behav. Sci.}{}%
  \ifthenelse{\equal{\@journal}{Bioengineering}}{Bioengineering}{}%
  \ifthenelse{\equal{\@journal}{Biology}}{Biology}{}%
  \ifthenelse{\equal{\@journal}{Biomedicines}}{Biomedicines}{}%
  \ifthenelse{\equal{\@journal}{Biomolecules}}{Biomolecules}{}%
  \ifthenelse{\equal{\@journal}{Biosensors}}{Biosensors}{}%
  \ifthenelse{\equal{\@journal}{BrainSciences}}{Brain Sci.}{}%
  \ifthenelse{\equal{\@journal}{Buildings}}{Buildings}{}%
  \ifthenelse{\equal{\@journal}{Cancers}}{Cancers}{}%
  \ifthenelse{\equal{\@journal}{Catalysts}}{Catalysts}{}%
  \ifthenelse{\equal{\@journal}{Cells}}{Cells}{}%
  \ifthenelse{\equal{\@journal}{Challenges}}{Challenges}{}%
  \ifthenelse{\equal{\@journal}{Chemosensors}}{Chemosensors}{}%
  \ifthenelse{\equal{\@journal}{Children}}{Children}{}%
  \ifthenelse{\equal{\@journal}{Chromatography}}{Chromatography}{}%
  \ifthenelse{\equal{\@journal}{Climate}}{Climate}{}%
  \ifthenelse{\equal{\@journal}{Coatings}}{Coatings}{}%
  \ifthenelse{\equal{\@journal}{Computation}}{Computation}{}%
  \ifthenelse{\equal{\@journal}{Computers}}{Computers}{}%
  \ifthenelse{\equal{\@journal}{Cosmetics}}{Cosmetics}{}%
  \ifthenelse{\equal{\@journal}{Crystals}}{Crystals}{}%
  \ifthenelse{\equal{\@journal}{DentistryJournal}}{Dent. J.}{}%
  \ifthenelse{\equal{\@journal}{Diagnostics}}{Diagnostics}{}%
  \ifthenelse{\equal{\@journal}{Diseases}}{Diseases}{}%
  \ifthenelse{\equal{\@journal}{Diversity}}{Diversity}{}%
  \ifthenelse{\equal{\@journal}{Econometrics}}{Econometrics}{}%
  \ifthenelse{\equal{\@journal}{Economies}}{Economies}{}%
  \ifthenelse{\equal{\@journal}{Education}}{Education}{}%
  \ifthenelse{\equal{\@journal}{Electronics}}{Electronics}{}%
  \ifthenelse{\equal{\@journal}{Energies}}{Energies}{}%
  \ifthenelse{\equal{\@journal}{Entropy}}{Entropy}{}%
  \ifthenelse{\equal{\@journal}{EnvironmentalSciences}}{Eviron. Sci.}{}%
  \ifthenelse{\equal{\@journal}{Fibers}}{Fibers}{}%
  \ifthenelse{\equal{\@journal}{Foods}}{Foods}{}%
  \ifthenelse{\equal{\@journal}{Forests}}{Forests}{}%
  \ifthenelse{\equal{\@journal}{FutureInternet}}{Future Internet}{}%
  \ifthenelse{\equal{\@journal}{Galaxies}}{Galaxies}{}%
  \ifthenelse{\equal{\@journal}{Games}}{Games}{}%
  \ifthenelse{\equal{\@journal}{Genes}}{Genes}{}%
  \ifthenelse{\equal{\@journal}{Geosciences}}{Geosciences}{}%
  \ifthenelse{\equal{\@journal}{Healthcare}}{Healthcare}{}%
  \ifthenelse{\equal{\@journal}{Humanities}}{Humanities}{}%
  \ifthenelse{\equal{\@journal}{Informatics}}{Informatics}{}%
  \ifthenelse{\equal{\@journal}{Information}}{Information}{}%
  \ifthenelse{\equal{\@journal}{Inorganics}}{Inorganics}{}%
  \ifthenelse{\equal{\@journal}{Insects}}{Insects}{}%
  \ifthenelse{\equal{\@journal}{Ijerph}}{Int. J. Environ. Res. Public Health}{}%
  \ifthenelse{\equal{\@journal}{Ijfs}}{Int. J. Financ. Stud.}{}%
  \ifthenelse{\equal{\@journal}{Ijms}}{Int. J. Mol. Sci.}{}%
  \ifthenelse{\equal{\@journal}{Ijgi}}{ISPRS Int. J. Geo-Inf.}{}%
  \ifthenelse{\equal{\@journal}{Jcdd}}{J. Cardiovasc. Dev. Dis.}{}%
  \ifthenelse{\equal{\@journal}{Jcm}}{J. Clinical Medicine}{}%
  \ifthenelse{\equal{\@journal}{Jdb}}{J. Dev. Biol.}{}%
  \ifthenelse{\equal{\@journal}{Jfb}}{J. Funct. Biomater.}{}%   
  \ifthenelse{\equal{\@journal}{Joi}}{J. Intell.}{}% 
  \ifthenelse{\equal{\@journal}{Jlpea}}{J. Low Power Electron. Appl.}{}%
  \ifthenelse{\equal{\@journal}{Jmse}}{J. Mar. Sci. Eng.}{}%??????
  \ifthenelse{\equal{\@journal}{Jpm}}{J. Pers. Med.}{}%
  \ifthenelse{\equal{\@journal}{Jrfm}}{J. Risk Financial Manag.}{}%
  \ifthenelse{\equal{\@journal}{Jsan}}{J. Sens. Actuator Netw.}{}%
  \ifthenelse{\equal{\@journal}{Land}}{Land}{}%
  \ifthenelse{\equal{\@journal}{Laws}}{Laws}{}%
  \ifthenelse{\equal{\@journal}{Life}}{Life}{}%
  \ifthenelse{\equal{\@journal}{Lubricants}}{Lubricants}{}%
  \ifthenelse{\equal{\@journal}{Machines}}{Machines}{}%
  \ifthenelse{\equal{\@journal}{MarineDrugs}}{Mar. Drugs}{}%
  \ifthenelse{\equal{\@journal}{Materials}}{Materials}{}%
  \ifthenelse{\equal{\@journal}{Mathematics}}{Mathematics}{}%
  \ifthenelse{\equal{\@journal}{MedicalSciences}}{Med. Sci.}{}% 
  \ifthenelse{\equal{\@journal}{Membranes}}{Membranes}{}%
  \ifthenelse{\equal{\@journal}{Metabolites}}{Metabolites}{}%
  \ifthenelse{\equal{\@journal}{Metals}}{Metals}{}%
  \ifthenelse{\equal{\@journal}{Microarrays}}{Microarrays}{}%
  \ifthenelse{\equal{\@journal}{Micromachines}}{Micromachines}{}%
  \ifthenelse{\equal{\@journal}{Microorganisms}}{Microorganisms}{}%
  \ifthenelse{\equal{\@journal}{Minerals}}{Minerals}{}%
  \ifthenelse{\equal{\@journal}{Molbank}}{Molbank}{}%
  \ifthenelse{\equal{\@journal}{Molecules}}{Molecules}{}%
  \ifthenelse{\equal{\@journal}{Nanomaterials}}{Nanomaterials}{}%
  \ifthenelse{\equal{\@journal}{Nutrients}}{Nutrients}{}%
  \ifthenelse{\equal{\@journal}{Optics}}{Optics}{}%
  \ifthenelse{\equal{\@journal}{Pathogens}}{Pathogens}{}%
  \ifthenelse{\equal{\@journal}{Pharmaceuticals}}{Pharmaceuticals}{}%
  \ifthenelse{\equal{\@journal}{Pharmaceutics}}{Pharmaceutics}{}%
  \ifthenelse{\equal{\@journal}{Pharmacy}}{Pharmacy}{}%
  \ifthenelse{\equal{\@journal}{Plants}}{Plants}{}%
  \ifthenelse{\equal{\@journal}{Polymers}}{Polymers}{}%
  \ifthenelse{\equal{\@journal}{Processes}}{Processes}{}%
  \ifthenelse{\equal{\@journal}{Proteomes}}{Proteomes}{}%
  \ifthenelse{\equal{\@journal}{Publications}}{Publications}{}%
  \ifthenelse{\equal{\@journal}{Religions}}{Religions}{}%
  \ifthenelse{\equal{\@journal}{RemoteSensing}}{Remote Sens.}{}%
  \ifthenelse{\equal{\@journal}{Resources}}{Resources}{}%
  \ifthenelse{\equal{\@journal}{Risks}}{Risks}{}%
  \ifthenelse{\equal{\@journal}{Robotics}}{Robotics}{}%
  \ifthenelse{\equal{\@journal}{Sensors}}{Sensors}{}%
  \ifthenelse{\equal{\@journal}{SocialSciences}}{Soc. Sci.}{}%
  \ifthenelse{\equal{\@journal}{Societies}}{Societies}{}%
  \ifthenelse{\equal{\@journal}{Sports}}{Sports}{}%
  \ifthenelse{\equal{\@journal}{Sustainability}}{Sustainability}{}%
  \ifthenelse{\equal{\@journal}{Symmetry}}{Symmetry}{}%
  \ifthenelse{\equal{\@journal}{Systems}}{Systems}{}%
  \ifthenelse{\equal{\@journal}{Technologies}}{Technologies}{}%
  \ifthenelse{\equal{\@journal}{Toxics}}{Toxics}{}%
  \ifthenelse{\equal{\@journal}{Toxins}}{Toxins}{}%
  \ifthenelse{\equal{\@journal}{Vaccines}}{Vaccines}{}%
  \ifthenelse{\equal{\@journal}{VeterinarySciences}}{Vet. Sci.}{}%
  \ifthenelse{\equal{\@journal}{Viruses}}{Viruses}{}%
  \ifthenelse{\equal{\@journal}{Water}}{Water}{}%
  }
  
%% Define the logo size of the journal using the ifthen package
\newcommand{\logosize}{%
  \ifthenelse{\equal{\@journal}{Actuators}}{\fontsize{32}{32}}{}%
  \ifthenelse{\equal{\@journal}{AdministrativeSciences}}{\fontsize{27}{27}}{}%
  \ifthenelse{\equal{\@journal}{Aerospace}}{\fontsize{32}{32}}{}%
  \ifthenelse{\equal{\@journal}{Agriculture}}{\fontsize{32}{32}}{}%
  \ifthenelse{\equal{\@journal}{Agronomy}}{\fontsize{32}{32}}{}%
  \ifthenelse{\equal{\@journal}{Algorithms}}{\fontsize{32}{32}}{}%
  \ifthenelse{\equal{\@journal}{Animals}}{\fontsize{32}{32}}{}%
  \ifthenelse{\equal{\@journal}{Antibiotics}}{\fontsize{32}{32}}{}%
  \ifthenelse{\equal{\@journal}{Antibodies}}{\fontsize{32}{32}}{}%
  \ifthenelse{\equal{\@journal}{Antioxidants}}{\fontsize{32}{32}}{}%
  \ifthenelse{\equal{\@journal}{AppliedSciences}}{\fontsize{28}{28}}{}%
  \ifthenelse{\equal{\@journal}{Arts}}{\fontsize{36}{36}}{}%
  \ifthenelse{\equal{\@journal}{Atmosphere}}{\fontsize{30}{30}}{}%
  \ifthenelse{\equal{\@journal}{Atoms}}{\fontsize{36}{36}}{}%
  \ifthenelse{\equal{\@journal}{Axioms}}{\fontsize{32}{32}}{}%
  \ifthenelse{\equal{\@journal}{BehavioralSciences}}{\fontsize{29}{29}}{}%
  \ifthenelse{\equal{\@journal}{Bioengineering}}{\fontsize{36}{36}}{}%
  \ifthenelse{\equal{\@journal}{Biology}}{\fontsize{36}{36}}{}%
  \ifthenelse{\equal{\@journal}{Biomedicines}}{\fontsize{28}{28}}{}%
  \ifthenelse{\equal{\@journal}{Biomolecules}}{\fontsize{32}{32}}{}%
  \ifthenelse{\equal{\@journal}{Biosensors}}{\fontsize{32}{32}}{}%
  \ifthenelse{\equal{\@journal}{BrainSciences}}{\fontsize{24}{24}}{}%
  \ifthenelse{\equal{\@journal}{Buildings}}{\fontsize{32}{32}}{}%
  \ifthenelse{\equal{\@journal}{Cancers}}{\fontsize{32}{32}}{}%
  \ifthenelse{\equal{\@journal}{Catalysts}}{\fontsize{36}{36}}{}%
  \ifthenelse{\equal{\@journal}{Cells}}{\fontsize{36}{36}}{}%
  \ifthenelse{\equal{\@journal}{Challenges}}{\fontsize{36}{36}}{}%
  \ifthenelse{\equal{\@journal}{Chemosensors}}{\fontsize{32}{32}}{}%
  \ifthenelse{\equal{\@journal}{Children}}{\fontsize{32}{32}}{}%
  \ifthenelse{\equal{\@journal}{Chromatography}}{\fontsize{26}{26}}{}%
  \ifthenelse{\equal{\@journal}{Climate}}{\fontsize{32}{32}}{}%
  \ifthenelse{\equal{\@journal}{Coatings}}{\fontsize{36}{36}}{}%
  \ifthenelse{\equal{\@journal}{Computation}}{\fontsize{32}{32}}{}%
  \ifthenelse{\equal{\@journal}{Computers}}{\fontsize{36}{36}}{}%
  \ifthenelse{\equal{\@journal}{Cosmetics}}{\fontsize{32}{32}}{}%
  \ifthenelse{\equal{\@journal}{Crystals}}{\fontsize{32}{32}}{}%
  \ifthenelse{\equal{\@journal}{DentistryJournal}}{\fontsize{25}{25}}{}%
  \ifthenelse{\equal{\@journal}{Diagnostics}}{\fontsize{32}{32}}{}%
  \ifthenelse{\equal{\@journal}{Diseases}}{\fontsize{32}{32}}{}%
  \ifthenelse{\equal{\@journal}{Diversity}}{\fontsize{32}{32}}{}%
  \ifthenelse{\equal{\@journal}{Econometrics}}{\fontsize{32}{32}}{}%
  \ifthenelse{\equal{\@journal}{Economies}}{\fontsize{32}{32}}{}%
  \ifthenelse{\equal{\@journal}{Education}}{\fontsize{29}{29}}{}%
  \ifthenelse{\equal{\@journal}{Electronics}}{\fontsize{36}{36}}{}%
  \ifthenelse{\equal{\@journal}{Energies}}{\fontsize{32}{32}}{}%
  \ifthenelse{\equal{\@journal}{Entropy}}{\fontsize{32}{32}}{}%
  \ifthenelse{\equal{\@journal}{EnvironmentalSciences}}{\fontsize{30}{30}}{}%
  \ifthenelse{\equal{\@journal}{Fibers}}{\fontsize{32}{32}}{}%
  \ifthenelse{\equal{\@journal}{Foods}}{\fontsize{38}{38}}{}%
  \ifthenelse{\equal{\@journal}{Forests}}{\fontsize{36}{36}}{}%
  \ifthenelse{\equal{\@journal}{FutureInternet}}{\fontsize{28}{28}}{}%
  \ifthenelse{\equal{\@journal}{Galaxies}}{\fontsize{36}{36}}{}%
  \ifthenelse{\equal{\@journal}{Games}}{\fontsize{32}{32}}{}%
  \ifthenelse{\equal{\@journal}{Genes}}{\fontsize{36}{36}}{}%
  \ifthenelse{\equal{\@journal}{Geosciences}}{\fontsize{36}{36}}{}%
  \ifthenelse{\equal{\@journal}{Healthcare}}{\fontsize{32}{32}}{}%
  \ifthenelse{\equal{\@journal}{Humanities}}{\fontsize{32}{32}}{}%
  \ifthenelse{\equal{\@journal}{Informatics}}{\fontsize{28}{28}}{}%
  \ifthenelse{\equal{\@journal}{Information}}{\fontsize{28}{28}}{}%
  \ifthenelse{\equal{\@journal}{Inorganics}}{\fontsize{36}{36}}{}%
  \ifthenelse{\equal{\@journal}{Insects}}{\fontsize{36}{36}}{}%
  \ifthenelse{\equal{\@journal}{Ijerph}}{\fontsize{13}{13}}{}%
  \ifthenelse{\equal{\@journal}{Ijfs}}{\fontsize{13}{13}}{}%
  \ifthenelse{\equal{\@journal}{Ijms}}{\fontsize{14}{14}}{}%
  \ifthenelse{\equal{\@journal}{Ijgi}}{\fontsize{16}{16}}{}%
  \ifthenelse{\equal{\@journal}{Jcdd}}{\fontsize{18}{18}}{}%
  \ifthenelse{\equal{\@journal}{Jcm}}{\fontsize{20}{20}}{}%
  \ifthenelse{\equal{\@journal}{Jdb}}{\fontsize{20}{20}}{}%
  \ifthenelse{\equal{\@journal}{Jfb}}{\fontsize{19}{19}}{}%
  \ifthenelse{\equal{\@journal}{Joi}}{\fontsize{19}{19}}{}%
  \ifthenelse{\equal{\@journal}{Jlpea}}{\fontsize{17}{17}}{}%
  \ifthenelse{\equal{\@journal}{Jmse}}{\fontsize{20}{20}}{}%
  \ifthenelse{\equal{\@journal}{Jpm}}{\fontsize{20}{20}}{}%
  \ifthenelse{\equal{\@journal}{Jrfm}}{\fontsize{13}{13}}{}%
  \ifthenelse{\equal{\@journal}{Jsan}}{\fontsize{18}{18}}{}%
  \ifthenelse{\equal{\@journal}{Land}}{\fontsize{32}{32}}{}%
  \ifthenelse{\equal{\@journal}{Laws}}{\fontsize{32}{32}}{}%
  \ifthenelse{\equal{\@journal}{Life}}{\fontsize{40}{40}}{}%
  \ifthenelse{\equal{\@journal}{Lubricants}}{\fontsize{36}{36}}{}%
  \ifthenelse{\equal{\@journal}{Machines}}{\fontsize{32}{32}}{}%
  \ifthenelse{\equal{\@journal}{MarineDrugs}}{\fontsize{22}{22}}{}%
  \ifthenelse{\equal{\@journal}{Materials}}{\fontsize{32}{32}}{}%
  \ifthenelse{\equal{\@journal}{Mathematics}}{\fontsize{32}{32}}{}%
  \ifthenelse{\equal{\@journal}{MedicalSciences}}{\fontsize{32}{32}}{}%
  \ifthenelse{\equal{\@journal}{Membranes}}{\fontsize{36}{36}}{}%
  \ifthenelse{\equal{\@journal}{Metabolites}}{\fontsize{36}{36}}{}%
  \ifthenelse{\equal{\@journal}{Metals}}{\fontsize{36}{36}}{}%
  \ifthenelse{\equal{\@journal}{Microarrays}}{\fontsize{32}{32}}{}%
  \ifthenelse{\equal{\@journal}{Micromachines}}{\fontsize{26}{26}}{}%
  \ifthenelse{\equal{\@journal}{Microorganisms}}{\fontsize{32}{32}}{}%
  \ifthenelse{\equal{\@journal}{Minerals}}{\fontsize{36}{36}}{}%
  \ifthenelse{\equal{\@journal}{Molbank}}{\fontsize{32}{32}}{}%
  \ifthenelse{\equal{\@journal}{Molecules}}{\fontsize{32}{32}}{}%
  \ifthenelse{\equal{\@journal}{Nanomaterials}}{\fontsize{26}{26}}{}%
  \ifthenelse{\equal{\@journal}{Nutrients}}{\fontsize{32}{32}}{}%
  \ifthenelse{\equal{\@journal}{Optics}}{\fontsize{32}{32}}{}%
  \ifthenelse{\equal{\@journal}{Pathogens}}{\fontsize{36}{36}}{}%
  \ifthenelse{\equal{\@journal}{Pharmaceuticals}}{\fontsize{26}{26}}{}%
  \ifthenelse{\equal{\@journal}{Pharmaceutics}}{\fontsize{26}{26}}{}%
  \ifthenelse{\equal{\@journal}{Pharmacy}}{\fontsize{28}{28}}{}%
  \ifthenelse{\equal{\@journal}{Plants}}{\fontsize{36}{36}}{}%
  \ifthenelse{\equal{\@journal}{Polymers}}{\fontsize{36}{36}}{}%
  \ifthenelse{\equal{\@journal}{Processes}}{\fontsize{32}{32}}{}%
  \ifthenelse{\equal{\@journal}{Proteomes}}{\fontsize{28}{28}}{}%
  \ifthenelse{\equal{\@journal}{Publications}}{\fontsize{32}{32}}{}%
  \ifthenelse{\equal{\@journal}{Religions}}{\fontsize{36}{36}}{}%
  \ifthenelse{\equal{\@journal}{RemoteSensing}}{\fontsize{24}{24}}{}%
  \ifthenelse{\equal{\@journal}{Resources}}{\fontsize{32}{32}}{}%
  \ifthenelse{\equal{\@journal}{Risks}}{\fontsize{38}{38}}{}%
  \ifthenelse{\equal{\@journal}{Robotics}}{\fontsize{32}{32}}{}%
  \ifthenelse{\equal{\@journal}{Sensors}}{\fontsize{32}{32}}{}%
  \ifthenelse{\equal{\@journal}{SocialSciences}}{\fontsize{24}{24}}{}%
  \ifthenelse{\equal{\@journal}{Societies}}{\fontsize{32}{32}}{}%
  \ifthenelse{\equal{\@journal}{Sports}}{\fontsize{36}{36}}{}%
  \ifthenelse{\equal{\@journal}{Sustainability}}{\fontsize{28}{28}}{}%
  \ifthenelse{\equal{\@journal}{Symmetry}}{\fontsize{32}{32}}{}%
  \ifthenelse{\equal{\@journal}{Systems}}{\fontsize{32}{32}}{}%
  \ifthenelse{\equal{\@journal}{Technologies}}{\fontsize{32}{32}}{}%
  \ifthenelse{\equal{\@journal}{Toxics}}{\fontsize{32}{32}}{}%
  \ifthenelse{\equal{\@journal}{Toxins}}{\fontsize{32}{32}}{}%
  \ifthenelse{\equal{\@journal}{Vaccines}}{\fontsize{36}{36}}{}%
  \ifthenelse{\equal{\@journal}{VeterinarySciences}}{\fontsize{30}{30}}{}%
  \ifthenelse{\equal{\@journal}{Viruses}}{\fontsize{32}{32}}{}%
  \ifthenelse{\equal{\@journal}{Water}}{\fontsize{36}{36}}{}%
  }
  
%% Creates the command that makes the logo of the journal.
\newcommand{\logo}{%
    \ifthenelse{\equal{\@journal}{Actuators}}{actuators}{}%
    \ifthenelse{\equal{\@journal}{AdministrativeSciences}}{\\[-42pt]administrative\\[10pt]sciences}{}%
    \ifthenelse{\equal{\@journal}{Aerospace}}{aerospace}{}%
    \ifthenelse{\equal{\@journal}{Agriculture}}{agriculture}{}%
    \ifthenelse{\equal{\@journal}{Agronomy}}{agronomy}{}%
    \ifthenelse{\equal{\@journal}{Algorithms}}{algorithms}{}%
    \ifthenelse{\equal{\@journal}{Animals}}{animals}{}%
    \ifthenelse{\equal{\@journal}{Antibiotics}}{antibiotics}{}%
    \ifthenelse{\equal{\@journal}{Antibodies}}{antibodies}{}%
    \ifthenelse{\equal{\@journal}{Antioxidants}}{antioxidants}{}%
    \ifthenelse{\equal{\@journal}{AppliedSciences}}{applied sciences}{}%
    \ifthenelse{\equal{\@journal}{Arts}}{arts}{}%
    \ifthenelse{\equal{\@journal}{Atmosphere}}{atmosphere}{}%
    \ifthenelse{\equal{\@journal}{Atoms}}{atoms}{}%
    \ifthenelse{\equal{\@journal}{Axioms}}{axioms}{}%
    \ifthenelse{\equal{\@journal}{BehavioralSciences}}{\\[-44pt]behavioral\\[10pt]sciences}{}%
    \ifthenelse{\equal{\@journal}{Bioengineering}}{bioengineering}{}%
    \ifthenelse{\equal{\@journal}{Biology}}{biology}{}%
    \ifthenelse{\equal{\@journal}{Biomedicines}}{biomedicines}{}%
    \ifthenelse{\equal{\@journal}{Biomolecules}}{biomolecules}{}%
    \ifthenelse{\equal{\@journal}{Biosensors}}{biosensors}{}%
    \ifthenelse{\equal{\@journal}{BrainSciences}}{brain sciences}{}%
    \ifthenelse{\equal{\@journal}{Buildings}}{buildings}{}%
    \ifthenelse{\equal{\@journal}{Cancers}}{cancers}{}%
    \ifthenelse{\equal{\@journal}{Catalysts}}{catalysts}{}%
    \ifthenelse{\equal{\@journal}{Cells}}{cells}{}%
    \ifthenelse{\equal{\@journal}{Challenges}}{challenges}{}%
    \ifthenelse{\equal{\@journal}{Chemosensors}}{chemosensors}{}%
    \ifthenelse{\equal{\@journal}{Children}}{children}{}%
    \ifthenelse{\equal{\@journal}{Chromatography}}{chromatography}{}%
    \ifthenelse{\equal{\@journal}{Climate}}{climate}{}%
    \ifthenelse{\equal{\@journal}{Coatings}}{coatings}{}%
    \ifthenelse{\equal{\@journal}{Computation}}{computation}{}%
    \ifthenelse{\equal{\@journal}{Computers}}{computers}{}%
    \ifthenelse{\equal{\@journal}{Cosmetics}}{cosmetics}{}%
    \ifthenelse{\equal{\@journal}{Crystals}}{crystals}{}%
    \ifthenelse{\equal{\@journal}{DentistryJournal}}{dentistry journal}{}%
    \ifthenelse{\equal{\@journal}{Diagnostics}}{diagnostics}{}%
    \ifthenelse{\equal{\@journal}{Diseases}}{diseases}{}%
    \ifthenelse{\equal{\@journal}{Diversity}}{diversity}{}%
    \ifthenelse{\equal{\@journal}{Econometrics}}{econometrics}{}%
    \ifthenelse{\equal{\@journal}{Economies}}{economies}{}%
    \ifthenelse{\equal{\@journal}{Education}}{\\[-48pt]education\\[4pt]sciences}{}%
    \ifthenelse{\equal{\@journal}{Electronics}}{electronics}{}%
    \ifthenelse{\equal{\@journal}{Energies}}{energies}{}%
    \ifthenelse{\equal{\@journal}{Entropy}}{entropy}{}%
    \ifthenelse{\equal{\@journal}{EnvironmentalSciences}}{evironmental\\sciences}{}%
    \ifthenelse{\equal{\@journal}{Fibers}}{f\mbox{}ibers}{}%
    \ifthenelse{\equal{\@journal}{Foods}}{foods}{}%
    \ifthenelse{\equal{\@journal}{Forests}}{forests}{}%
    \ifthenelse{\equal{\@journal}{FutureInternet}}{future internet}{}%
    \ifthenelse{\equal{\@journal}{Galaxies}}{galaxies}{}%
    \ifthenelse{\equal{\@journal}{Games}}{games}{}%
    \ifthenelse{\equal{\@journal}{Genes}}{genes}{}%
    \ifthenelse{\equal{\@journal}{Geosciences}}{geosciences}{}%
    \ifthenelse{\equal{\@journal}{Healthcare}}{healthcare}{}%   
    \ifthenelse{\equal{\@journal}{Humanities}}{humanities}{}%
    \ifthenelse{\equal{\@journal}{Informatics}}{informatics}{}%
    \ifthenelse{\equal{\@journal}{Information}}{information}{}%
    \ifthenelse{\equal{\@journal}{Inorganics}}{inorganics}{}%
    \ifthenelse{\equal{\@journal}{Insects}}{insects}{}%
    \ifthenelse{\equal{\@journal}{Ijerph}}{\normalfont{International Journal of}\\\normalfont\bfseries{Environmental Research and\\Public Health}}{}%
    \ifthenelse{\equal{\@journal}{Ijfs}}	{\normalfont\fontsize{18}{18}{International Journal of}\\\normalfont\bfseries\itshape{Financial Studies}}{}%
    \ifthenelse{\equal{\@journal}{Ijms}}{\normalfont\fontsize{18}{18}{International Journal of}\\[4pt]\normalfont\bfseries\selectfont{Molecular Sciences}}{}%
    \ifthenelse{\equal{\@journal}{Ijgi}}	{\normalfont{\color{black}{ISPRS International Journal of}}\\[9pt]\normalfont\itshape\bfseries\fontsize{25}{25}\selectfont{Geo-Information}}{}%
    \ifthenelse{\equal{\@journal}{Jcdd}}{\normalfont\fontsize{20}{20}{Journal of}\\\normalfont\bfseries\itshape\selectfont{Cardiovascular\\[6pt] Development and Disease}}{}%
    \ifthenelse{\equal{\@journal}{Jcm}}{\normalfont\fontsize{22}{22}{Journal of}\\[6pt]\normalfont\bfseries\itshape\selectfont{Clinical Medicine}}{}%
    \ifthenelse{\equal{\@journal}{Jdb}}{\normalfont{Journal of}\\\normalfont\bfseries\itshape\fontsize{23}{20}\selectfont{Developmental}\\[4pt]\normalfont\bfseries\itshape\fontsize{23}{23}\selectfont{Biology}}{}%
    \ifthenelse{\equal{\@journal}{Jfb}}{\normalfont\fontsize{23}{23}{Journal of}\\\normalfont\bfseries\itshape\selectfont{Functional}\\[8pt]\normalfont\itshape\bfseries\selectfont{Biomaterials}}{}%
    \ifthenelse{\equal{\@journal}{Joi}}{\normalfont\fontsize{27}{27}{Journal of}\\[8pt]\normalfont\bfseries\itshape\selectfont{Intelligence}}{}%
    \ifthenelse{\equal{\@journal}{Jlpea}}{\normalfont\fontsize{19}{19}{Journal of}\\[-4pt]\normalfont\bfseries\selectfont\itshape{Low Power Electronics}\\[4pt]\normalfont\bfseries\selectfont\itshape{and Applications}}{}%
    \ifthenelse{\equal{\@journal}{Jmse}}{\normalfont\fontsize{22}{22}{\color{black}{Journal of}}\\\normalfont\itshape\bfseries\selectfont{Marine Science\\[6pt] and Engineering}}{}%
    \ifthenelse{\equal{\@journal}{Jpm}}{\normalfont\fontsize{22}{22}{Journal of}\\\normalfont\bfseries\itshape\selectfont{Personalized\\[6pt] Medicine}}{}%
    \ifthenelse{\equal{\@journal}{Jrfm}}{\normalfont\fontsize{18}{18}{Journal of}\\\normalfont\bfseries\itshape\selectfont{Risk and Financial\\Management}}{}%
    \ifthenelse{\equal{\@journal}{Jsan}}{\bfseries\itshape{Journal of Sensor\\[6pt] and Actuator Networks}}{}%
    \ifthenelse{\equal{\@journal}{Land}}{land}{}%
    \ifthenelse{\equal{\@journal}{Laws}}{laws}{}%
    \ifthenelse{\equal{\@journal}{Life}}{life}{}%
    \ifthenelse{\equal{\@journal}{Lubricants}}{lubricants}{}%
    \ifthenelse{\equal{\@journal}{Machines}}{machines}{}%
    \ifthenelse{\equal{\@journal}{MarineDrugs}}{marine drugs}{}%
    \ifthenelse{\equal{\@journal}{Materials}}{materials}{}%
    \ifthenelse{\equal{\@journal}{Mathematics}}{mathematics}{}%
    \ifthenelse{\equal{\@journal}{MedicalSciences}}{medical sciences}{}%
    \ifthenelse{\equal{\@journal}{Membranes}}{membranes}{}%
    \ifthenelse{\equal{\@journal}{Metabolites}}{metabolites}{}%
    \ifthenelse{\equal{\@journal}{Metals}}{metals}{}%
    \ifthenelse{\equal{\@journal}{Microarrays}}{microarrays}{}%
    \ifthenelse{\equal{\@journal}{Micromachines}}{micromachines}{}%
    \ifthenelse{\equal{\@journal}{Microorganisms}}{microorganisms}{}%
    \ifthenelse{\equal{\@journal}{Minerals}}{minerals}{}%
    \ifthenelse{\equal{\@journal}{Molbank}}{molbank}{}%
    \ifthenelse{\equal{\@journal}{Molecules}}{molecules}{}%
    \ifthenelse{\equal{\@journal}{Nanomaterials}}{nanomaterials}{}%
    \ifthenelse{\equal{\@journal}{Nutrients}}{nutrients}{}%
    \ifthenelse{\equal{\@journal}{Optics}}{optics}{}%
    \ifthenelse{\equal{\@journal}{Pathogens}}{pathogens}{}%
    \ifthenelse{\equal{\@journal}{Pharmaceuticals}}{pharmaceuticals}{}%
    \ifthenelse{\equal{\@journal}{Pharmaceutics}}{pharmaceutics}{}%
    \ifthenelse{\equal{\@journal}{Pharmacy}}{pharmacy}{}%
    \ifthenelse{\equal{\@journal}{Plants}}{plants}{}%
    \ifthenelse{\equal{\@journal}{Polymers}}{polymers}{}%
    \ifthenelse{\equal{\@journal}{Processes}}{processes}{}%
    \ifthenelse{\equal{\@journal}{Proteomes}}{proteomes}{}%
    \ifthenelse{\equal{\@journal}{Publications}}{publications}{}%
    \ifthenelse{\equal{\@journal}{Religions}}{religions}{}%
    \ifthenelse{\equal{\@journal}{RemoteSensing}}{remote sensing}{}%
    \ifthenelse{\equal{\@journal}{Resources}}{resources}{}%
    \ifthenelse{\equal{\@journal}{Risks}}{risks}{}%
    \ifthenelse{\equal{\@journal}{Robotics}}{robotics}{}%
    \ifthenelse{\equal{\@journal}{Sensors}}{sensors}{}%
    \ifthenelse{\equal{\@journal}{SocialSciences}}{social sciences}{}%
    \ifthenelse{\equal{\@journal}{Societies}}{societies}{}%
    \ifthenelse{\equal{\@journal}{Sports}}{\vspace{1.2 mm}sports}{}%
    \ifthenelse{\equal{\@journal}{Sustainability}}{sustainability}{}%
    \ifthenelse{\equal{\@journal}{Symmetry}}{symmetry}{}%
    \ifthenelse{\equal{\@journal}{Systems}}{systems}{}%
    \ifthenelse{\equal{\@journal}{Technologies}}{technologies}{}%
    \ifthenelse{\equal{\@journal}{Toxics}}{toxics}{}%
    \ifthenelse{\equal{\@journal}{Toxins}}{toxins}{}%
    \ifthenelse{\equal{\@journal}{Vaccines}}{vaccines}{}%
    \ifthenelse{\equal{\@journal}{VeterinarySciences}}{veterinary sciences}{}%
    \ifthenelse{\equal{\@journal}{Viruses}}{viruses}{}%
    \ifthenelse{\equal{\@journal}{Water}}{water}{}%
}

%% Creates the ISSN for the journal using the ifthen package
\newcommand{\issn}{%
  \ifthenelse{\equal{\@journal}{Actuators}}{2076-0825}{}%
  \ifthenelse{\equal{\@journal}{AdministrativeSciences}}{2076-3387}{}%
  \ifthenelse{\equal{\@journal}{Aerospace}}{2226-4310}{}%
  \ifthenelse{\equal{\@journal}{Agriculture}}{2077-0472}{}%
  \ifthenelse{\equal{\@journal}{Agronomy}}{2073-4395}{}%
  \ifthenelse{\equal{\@journal}{Algorithms}}{1999-4893}{}%
  \ifthenelse{\equal{\@journal}{Animals}}{2076-2615}{}%
  \ifthenelse{\equal{\@journal}{Antibiotics}}{2079-6382}{}%
  \ifthenelse{\equal{\@journal}{Antibodies}}{2073-4468}{}%
  \ifthenelse{\equal{\@journal}{Antioxidants}}{2076-3921}{}%
  \ifthenelse{\equal{\@journal}{AppliedSciences}}{2076-3417}{}%
  \ifthenelse{\equal{\@journal}{Arts}}{2076-0752}{}%
  \ifthenelse{\equal{\@journal}{Atmosphere}}{2073-4433}{}%
  \ifthenelse{\equal{\@journal}{Atoms}}{2218-2004}{}%
  \ifthenelse{\equal{\@journal}{Axioms}}{2075-1680}{}%
  \ifthenelse{\equal{\@journal}{BehavioralSciences}}{2076-328X}{}%
  \ifthenelse{\equal{\@journal}{Bioengineering}}{2306-5354}{}%
  \ifthenelse{\equal{\@journal}{Biology}}{2079-7737}{}%
  \ifthenelse{\equal{\@journal}{Biomedicines}}{2075-4426}{}%
  \ifthenelse{\equal{\@journal}{Biomolecules}}{2218-273X}{}%
  \ifthenelse{\equal{\@journal}{Biosensors}}{2079-6374}{}%
  \ifthenelse{\equal{\@journal}{BrainSciences}}{2076-3425}{}%
  \ifthenelse{\equal{\@journal}{Buildings}}{2075-5309}{}%
  \ifthenelse{\equal{\@journal}{Cancers}}{2072-6694}{}%
  \ifthenelse{\equal{\@journal}{Catalysts}}{2073-4344}{}%
  \ifthenelse{\equal{\@journal}{Cells}}{2073-4409}{}%
  \ifthenelse{\equal{\@journal}{Challenges}}{2078-1547}{}%
  \ifthenelse{\equal{\@journal}{Chemosensors}}{2227-9040}{}%
  \ifthenelse{\equal{\@journal}{Children}}{2075-4426}{}%
  \ifthenelse{\equal{\@journal}{Chromatography}}{2227-9075}{}%
  \ifthenelse{\equal{\@journal}{Climate}}{2225-1154}{}%
  \ifthenelse{\equal{\@journal}{Coatings}}{2079-6412}{}%
  \ifthenelse{\equal{\@journal}{Computation}}{2079-3197}{}%
  \ifthenelse{\equal{\@journal}{Computers}}{2073-431X}{}%
  \ifthenelse{\equal{\@journal}{Cosmetics}}{2079-9284}{}%
  \ifthenelse{\equal{\@journal}{Crystals}}{2073-4352}{}%
  \ifthenelse{\equal{\@journal}{DentistryJournal}}{2304-6767}{}%
  \ifthenelse{\equal{\@journal}{Diagnostics}}{2075-4418}{}%
  \ifthenelse{\equal{\@journal}{Diseases}}{2079-9721}{}%
  \ifthenelse{\equal{\@journal}{Diversity}}{1424-2818}{}%
  \ifthenelse{\equal{\@journal}{Econometrics}}{2225-1146}{}%
  \ifthenelse{\equal{\@journal}{Economies}}{2227-7099}{}%
  \ifthenelse{\equal{\@journal}{Education}}{2076-3344}{}%
  \ifthenelse{\equal{\@journal}{Electronics}}{2079-9292}{}%
  \ifthenelse{\equal{\@journal}{Energies}}{1996-1073}{}%
  \ifthenelse{\equal{\@journal}{Entropy}}{1099-4300}{}%
  \ifthenelse{\equal{\@journal}{EnvironmentalSciences}}{2076-3298}{}%
  \ifthenelse{\equal{\@journal}{Fibers}}{2079-6439}{}%
  \ifthenelse{\equal{\@journal}{Foods}}{2304-8158}{}%
  \ifthenelse{\equal{\@journal}{Forests}}{1999-4907}{}%
  \ifthenelse{\equal{\@journal}{FutureInternet}}{1999-5903}{}%
  \ifthenelse{\equal{\@journal}{Galaxies}}{2075-4434}{}%
  \ifthenelse{\equal{\@journal}{Games}}{2073-4336}{}%
  \ifthenelse{\equal{\@journal}{Genes}}{2073-4425}{}%
  \ifthenelse{\equal{\@journal}{Geosciences}}{2076-3263}{}%
  \ifthenelse{\equal{\@journal}{Healthcare}}{2227-9032}{}%
  \ifthenelse{\equal{\@journal}{Humanities}}{2076-0787}{}%
  \ifthenelse{\equal{\@journal}{Informatics}}{2227-9709}{}%
  \ifthenelse{\equal{\@journal}{Information}}{2078-2489}{}%
  \ifthenelse{\equal{\@journal}{Inorganics}}{2304-6740}{}%
  \ifthenelse{\equal{\@journal}{Insects}}{2075-4450}{}%
  \ifthenelse{\equal{\@journal}{Ijerph}}{1660-4601}{}%
  \ifthenelse{\equal{\@journal}{Ijfs}}{2227-7072}{}%
  \ifthenelse{\equal{\@journal}{Ijms}}{1422-0067}{}%
  \ifthenelse{\equal{\@journal}{Ijgi}}{2220-9964}{}%
  \ifthenelse{\equal{\@journal}{Jcdd}}{2308-3425}{}%
  \ifthenelse{\equal{\@journal}{Jcm}}{2077-0383}{}%
  \ifthenelse{\equal{\@journal}{Jdb}}{2221-3759}{}%
  \ifthenelse{\equal{\@journal}{Jfb}}{2079-4983}{}%
  \ifthenelse{\equal{\@journal}{Joi}}{2079-3200}{}%
  \ifthenelse{\equal{\@journal}{Jlpea}}{2079-9268}{}%
  \ifthenelse{\equal{\@journal}{Jmse}}{2077-1312}{}%
  \ifthenelse{\equal{\@journal}{Jpm}}{2075-4426}{}%
  \ifthenelse{\equal{\@journal}{Jrfm}}{1911-8074}{}%
  \ifthenelse{\equal{\@journal}{Jsan}}{2224-2708}{}%
  \ifthenelse{\equal{\@journal}{Land}}{2073-445X}{}%
  \ifthenelse{\equal{\@journal}{Laws}}{2075-471X}{}%
  \ifthenelse{\equal{\@journal}{Life}}{2075-1729}{}%
  \ifthenelse{\equal{\@journal}{Lubricants}}{2075-4442}{}%
  \ifthenelse{\equal{\@journal}{Machines}}{2075-1702}{}%
  \ifthenelse{\equal{\@journal}{MarineDrugs}}{1660-3397}{}%
  \ifthenelse{\equal{\@journal}{Materials}}{1996-1944}{}%
  \ifthenelse{\equal{\@journal}{Mathematics}}{2227-7390}{}%
  \ifthenelse{\equal{\@journal}{MedicalSciences}}{2076-3271}{}%
  \ifthenelse{\equal{\@journal}{Membranes}}{2077-0375}{}%
  \ifthenelse{\equal{\@journal}{Metabolites}}{2218-1989}{}%
  \ifthenelse{\equal{\@journal}{Metals}}{2075-4701}{}%
  \ifthenelse{\equal{\@journal}{Microarrays}}{2076-3905}{}%
  \ifthenelse{\equal{\@journal}{Micromachines}}{2072-666X}{}%
  \ifthenelse{\equal{\@journal}{Microorganisms}}{2076-2607}{}%
  \ifthenelse{\equal{\@journal}{Minerals}}{2075-163X}{}%
  \ifthenelse{\equal{\@journal}{Molbank}}{1422-8599}{}%
  \ifthenelse{\equal{\@journal}{Molecules}}{1420-3049}{}%
  \ifthenelse{\equal{\@journal}{Nanomaterials}}{2079-4991}{}%
  \ifthenelse{\equal{\@journal}{Nutrients}}{2072-6643}{}%
  \ifthenelse{\equal{\@journal}{Optics}}{2304-6732}{}%
  \ifthenelse{\equal{\@journal}{Pathogens}}{2076-0817}{}%
  \ifthenelse{\equal{\@journal}{Pharmaceuticals}}{1424-8247}{}%
  \ifthenelse{\equal{\@journal}{Pharmaceutics}}{1999-4923}{}%
  \ifthenelse{\equal{\@journal}{Pharmacy}}{2226-4787}{}%
  \ifthenelse{\equal{\@journal}{Plants}}{2223-7747}{}%
  \ifthenelse{\equal{\@journal}{Polymers}}{2073-4360}{}%
  \ifthenelse{\equal{\@journal}{Processes}}{2227-9717}{}%
  \ifthenelse{\equal{\@journal}{Proteomes}}{2227-7382}{}%
  \ifthenelse{\equal{\@journal}{Publications}}{2304-6775}{}%
  \ifthenelse{\equal{\@journal}{Religions}}{2077-1444}{}%
  \ifthenelse{\equal{\@journal}{RemoteSensing}}{2072-4292}{}%
  \ifthenelse{\equal{\@journal}{Resources}}{2079-9276}{}%
  \ifthenelse{\equal{\@journal}{Risks}}{2227-9091}{}%
  \ifthenelse{\equal{\@journal}{Robotics}}{2218-6581}{}%
  \ifthenelse{\equal{\@journal}{Sensors}}{1424-8220}{}%
  \ifthenelse{\equal{\@journal}{SocialSciences}}{2076-0760}{}%
  \ifthenelse{\equal{\@journal}{Societies}}{2075-4698}{}%
  \ifthenelse{\equal{\@journal}{Sports}}{2075-4663}{}%
  \ifthenelse{\equal{\@journal}{Sustainability}}{2071-1050}{}%
  \ifthenelse{\equal{\@journal}{Symmetry}}{2073-8994}{}%
  \ifthenelse{\equal{\@journal}{Systems}}{2079-8954}{}%
  \ifthenelse{\equal{\@journal}{Technologies}}{2227-7080}{}%
  \ifthenelse{\equal{\@journal}{Toxics}}{2305-6304}{}%
  \ifthenelse{\equal{\@journal}{Toxins}}{2072-6651}{}%
  \ifthenelse{\equal{\@journal}{Vaccines}}{2076-393X}{}%
  \ifthenelse{\equal{\@journal}{VeterinarySciences}}{2306-7381}{}%
  \ifthenelse{\equal{\@journal}{Viruses}}{1999-4915}{}%
  \ifthenelse{\equal{\@journal}{Water}}{2073-4441}{}%
  }
  
%% www.mdpi.com/journal/\logo
\newcommand{\web}{%
  \ifthenelse{\equal{\@journal}{Actuators}}{www.mdpi.com/journal/actuators}{}%
  \ifthenelse{\equal{\@journal}{AdministrativeSciences}}{www.mdpi.com/journal/admsci}{}%
  \ifthenelse{\equal{\@journal}{Aerospace}}{www.mdpi.com/journal/aerospace}{}%
  \ifthenelse{\equal{\@journal}{Agriculture}}{www.mdpi.com/journal/agriculture}{}%
  \ifthenelse{\equal{\@journal}{Agronomy}}{www.mdpi.com/journal/agronomy}{}%
  \ifthenelse{\equal{\@journal}{Algorithms}}{www.mdpi.com/journal/algorithms}{}%
  \ifthenelse{\equal{\@journal}{Animals}}{www.mdpi.com/journal/animals}{}%
  \ifthenelse{\equal{\@journal}{Antibiotics}}{http://www.mdpi.com/journal/antibiotics}{}%
  \ifthenelse{\equal{\@journal}{Antibodies}}{www.mdpi.com/journal/antibodies}{}%
  \ifthenelse{\equal{\@journal}{Antioxidants}}{www.mdpi.com/journal/antioxidants}{}%
  \ifthenelse{\equal{\@journal}{AppliedSciences}}{www.mdpi.com/journal/applsci}{}%
  \ifthenelse{\equal{\@journal}{Arts}}{http://www.mdpi.com/journal/arts}{}%
  \ifthenelse{\equal{\@journal}{Atmosphere}}{www.mdpi.com/journal/atmosphere}{}%
  \ifthenelse{\equal{\@journal}{Atoms}}{www.mdpi.com/journal/atoms}{}%
  \ifthenelse{\equal{\@journal}{Axioms}}{www.mdpi.com/journal/axioms}{}%
  \ifthenelse{\equal{\@journal}{BehavioralSciences}}{www.mdpi.com/journal/behavsci}{}%
  \ifthenelse{\equal{\@journal}{Bioengineering}}{www.mdpi.com/journal/bioengineering}{}%
  \ifthenelse{\equal{\@journal}{Biology}}{www.mdpi.com/journal/biology}{}%
  \ifthenelse{\equal{\@journal}{Biomedicines}}{www.mdpi.com/journal/biomedicines}{}%
  \ifthenelse{\equal{\@journal}{Biomolecules}}{www.mdpi.com/journal/biomolecules}{}%
  \ifthenelse{\equal{\@journal}{Biosensors}}{www.mdpi.com/journal/biosensors}{}%
  \ifthenelse{\equal{\@journal}{BrainSciences}}{www.mdpi.com/journal/brainsci}{}%
  \ifthenelse{\equal{\@journal}{Buildings}}{www.mdpi.com/journal/buildings}{}%
  \ifthenelse{\equal{\@journal}{Cancers}}{www.mdpi.com/journal/cancers}{}%
  \ifthenelse{\equal{\@journal}{Catalysts}}{www.mdpi.com/journal/catalysts}{}%
  \ifthenelse{\equal{\@journal}{Cells}}{www.mdpi.com/journal/cells}{}%
  \ifthenelse{\equal{\@journal}{Challenges}}{www.mdpi.com/journal/challenges}{}%
  \ifthenelse{\equal{\@journal}{Chemosensors}}{www.mdpi.com/journal/chemosensors}{}%
  \ifthenelse{\equal{\@journal}{Children}}{www.mdpi.com/journal/children}{}%
  \ifthenelse{\equal{\@journal}{Chromatography}}{www.mdpi.com/journal/chromatography}{}%
  \ifthenelse{\equal{\@journal}{Climate}}{www.mdpi.com/journal/climate}{}%
  \ifthenelse{\equal{\@journal}{Coatings}}{www.mdpi.com/journal/coatings}{}%
  \ifthenelse{\equal{\@journal}{Computation}}{www.mdpi.com/journal/computation}{}%
  \ifthenelse{\equal{\@journal}{Computers}}{www.mdpi.com/journal/computers}{}%
  \ifthenelse{\equal{\@journal}{Cosmetics}}{www.mdpi.com/journal/cosmetics}{}%
  \ifthenelse{\equal{\@journal}{Crystals}}{www.mdpi.com/journal/crystals}{}%
  \ifthenelse{\equal{\@journal}{Crystals}}{http://www.mdpi.com/journal/dentistry}{}%
  \ifthenelse{\equal{\@journal}{Diagnostics}}{www.mdpi.com/journal/diagnostics}{}%
  \ifthenelse{\equal{\@journal}{Diseases}}{www.mdpi.com/journal/diseases}{}%
  \ifthenelse{\equal{\@journal}{Diversity}}{www.mdpi.com/journal/diversity}{}%
  \ifthenelse{\equal{\@journal}{Econometrics}}{www.mdpi.com/journal/econometrics}{}%
  \ifthenelse{\equal{\@journal}{Economies}}{www.mdpi.com/journal/economies}{}%
  \ifthenelse{\equal{\@journal}{Education}}{www.mdpi.com/journal/education}{}%
  \ifthenelse{\equal{\@journal}{Electronics}}{www.mdpi.com/journal/electronics}{}%
  \ifthenelse{\equal{\@journal}{Energies}}{www.mdpi.com/journal/energies}{}%
  \ifthenelse{\equal{\@journal}{Entropy}}{www.mdpi.com/journal/entropy}{}%
  \ifthenelse{\equal{\@journal}{EnvironmentalSciences}}{www.mdpi.com/journal/environsci}{}%
  \ifthenelse{\equal{\@journal}{Fibers}}{www.mdpi.com/journal/f\mbox{}ibers}{}%
  \ifthenelse{\equal{\@journal}{Foods}}{www.mdpi.com/journal/foods}{}%
  \ifthenelse{\equal{\@journal}{Forests}}{www.mdpi.com/journal/forests}{}%
  \ifthenelse{\equal{\@journal}{FutureInternet}}{www.mdpi.com/journal/futureinternet}{}%
  \ifthenelse{\equal{\@journal}{Galaxies}}{www.mdpi.com/journal/galaxies}{}%
  \ifthenelse{\equal{\@journal}{Games}}{www.mdpi.com/journal/games}{}%
  \ifthenelse{\equal{\@journal}{Genes}}{www.mdpi.com/journal/genes}{}%
  \ifthenelse{\equal{\@journal}{Geosciences}}{www.mdpi.com/journal/geosciences}{}%
  \ifthenelse{\equal{\@journal}{Healthcare}}{http://www.mdpi.com/journal/healthcare}{}%
  \ifthenelse{\equal{\@journal}{Humanities}}{www.mdpi.com/journal/humanities}{}%
  \ifthenelse{\equal{\@journal}{Informatics}}{www.mdpi.com/journal/informatics}{}%
  \ifthenelse{\equal{\@journal}{Information}}{www.mdpi.com/journal/information}{}%
  \ifthenelse{\equal{\@journal}{Inorganics}}{www.mdpi.com/journal/inorganics}{}%
  \ifthenelse{\equal{\@journal}{Insects}}{www.mdpi.com/journal/insects}{}%
  \ifthenelse{\equal{\@journal}{Ijerph}}{www.mdpi.com/journal/ijerph}{}%
  \ifthenelse{\equal{\@journal}{Ijfs}}{www.mdpi.com/journal/ijfs}{}%
  \ifthenelse{\equal{\@journal}{Ijms}}{www.mdpi.com/journal/ijms}{}%
  \ifthenelse{\equal{\@journal}{Ijgi}}{www.mdpi.com/journal/ijgi}{}%
  \ifthenelse{\equal{\@journal}{Jcdd}}{http://www.mdpi.com/journal/jcdd}{}%
  \ifthenelse{\equal{\@journal}{Jcm}}{www.mdpi.com/journal/jcm}{}%
  \ifthenelse{\equal{\@journal}{Jdb}}{www.mdpi.com/journal/jdb}{}%
  \ifthenelse{\equal{\@journal}{Jfb}}{www.mdpi.com/journal/jfb}{}%
  \ifthenelse{\equal{\@journal}{Joi}}{http://www.mdpi.com/journal/jintelligence}{}%
  \ifthenelse{\equal{\@journal}{Jlpea}}{www.mdpi.com/journal/jlpea}{}%
  \ifthenelse{\equal{\@journal}{Jmse}}{www.mdpi.com/journal/jmse}{}%
  \ifthenelse{\equal{\@journal}{Jpm}}{www.mdpi.com/journal/jpm}{}%
  \ifthenelse{\equal{\@journal}{Jrfm}}{www.mdpi.com/journal/jrfm}{}%
  \ifthenelse{\equal{\@journal}{Jsan}}{www.mdpi.com/journal/jsan}{}%
  \ifthenelse{\equal{\@journal}{Land}}{www.mdpi.com/journal/land}{}%
  \ifthenelse{\equal{\@journal}{Laws}}{www.mdpi.com/journal/laws}{}%
  \ifthenelse{\equal{\@journal}{Life}}{www.mdpi.com/journal/life}{}%
  \ifthenelse{\equal{\@journal}{Lubricants}}{www.mdpi.com/journal/lubricants}{}%
  \ifthenelse{\equal{\@journal}{Machines}}{www.mdpi.com/journal/machines}{}%
  \ifthenelse{\equal{\@journal}{MarineDrugs}}{www.mdpi.com/journal/marinedrugs}{}%
  \ifthenelse{\equal{\@journal}{Materials}}{www.mdpi.com/journal/materials}{}%
  \ifthenelse{\equal{\@journal}{Mathematics}}{www.mdpi.com/journal/mathematics}{}%
  \ifthenelse{\equal{\@journal}{MedicalSciences}}{http://www.mdpi.com/journal/medsci}{}%
  \ifthenelse{\equal{\@journal}{Membranes}}{www.mdpi.com/journal/membranes}{}%
  \ifthenelse{\equal{\@journal}{Metabolites}}{www.mdpi.com/journal/metabolites}{}%
  \ifthenelse{\equal{\@journal}{Metals}}{www.mdpi.com/journal/metals}{}%
  \ifthenelse{\equal{\@journal}{Microarrays}}{www.mdpi.com/journal/microarrays}{}%
  \ifthenelse{\equal{\@journal}{Micromachines}}{www.mdpi.com/journal/micromachines}{}%
  \ifthenelse{\equal{\@journal}{Microorganisms}}{www.mdpi.com/journal/microorganisms}{}%
  \ifthenelse{\equal{\@journal}{Minerals}}{www.mdpi.com/journal/minerals}{}%
  \ifthenelse{\equal{\@journal}{Molbank}}{www.mdpi.com/journal/molbank}{}%
  \ifthenelse{\equal{\@journal}{Molecules}}{www.mdpi.com/journal/molecules}{}%
  \ifthenelse{\equal{\@journal}{Nanomaterials}}{www.mdpi.com/journal/nanomaterials}{}%
  \ifthenelse{\equal{\@journal}{Nutrients}}{www.mdpi.com/journal/nutrients}{}%
  \ifthenelse{\equal{\@journal}{Optics}}{www.mdpi.com/journal/optics}{}%
  \ifthenelse{\equal{\@journal}{Pathogens}}{www.mdpi.com/journal/pathogens}{}%
  \ifthenelse{\equal{\@journal}{Pharmaceuticals}}{www.mdpi.com/journal/pharmaceuticals}{}%
  \ifthenelse{\equal{\@journal}{Pharmaceutics}}{www.mdpi.com/journal/pharmaceutics}{}%
  \ifthenelse{\equal{\@journal}{Pharmacy}}{http://www.mdpi.com/journal/pharmacy}{}%
  \ifthenelse{\equal{\@journal}{Plants}}{www.mdpi.com/journal/plants}{}%
  \ifthenelse{\equal{\@journal}{Polymers}}{www.mdpi.com/journal/polymers}{}%
  \ifthenelse{\equal{\@journal}{Processes}}{www.mdpi.com/journal/processes}{}%
  \ifthenelse{\equal{\@journal}{Proteomes}}{www.mdpi.com/journal/proteomes}{}%
  \ifthenelse{\equal{\@journal}{Publications}}{www.mdpi.com/journal/publications}{}%
  \ifthenelse{\equal{\@journal}{Religions}}{www.mdpi.com/journal/religions}{}%
  \ifthenelse{\equal{\@journal}{RemoteSensing}}{www.mdpi.com/journal/remotesensing}{}%
  \ifthenelse{\equal{\@journal}{Resources}}{www.mdpi.com/journal/resources}{}%
  \ifthenelse{\equal{\@journal}{Risks}}{www.mdpi.com/journal/risks}{}%
  \ifthenelse{\equal{\@journal}{Robotics}}{www.mdpi.com/journal/robotics}{}%
  \ifthenelse{\equal{\@journal}{Sensors}}{www.mdpi.com/journal/sensors}{}%
  \ifthenelse{\equal{\@journal}{SocialSciences}}{www.mdpi.com/journal/socsci}{}%
  \ifthenelse{\equal{\@journal}{Societies}}{www.mdpi.com/journal/societies}{}%
  \ifthenelse{\equal{\@journal}{Sports}}{www.mdpi.com/journal/sports}{}%
  \ifthenelse{\equal{\@journal}{Sustainability}}{www.mdpi.com/journal/sustainability}{}%
  \ifthenelse{\equal{\@journal}{Symmetry}}{www.mdpi.com/journal/symmetry}{}%
  \ifthenelse{\equal{\@journal}{Systems}}{www.mdpi.com/journal/systems}{}%
  \ifthenelse{\equal{\@journal}{Technologies}}{www.mdpi.com/journal/technologies}{}%
  \ifthenelse{\equal{\@journal}{Toxics}}{http://www.mdpi.com/journal/toxics}{}%
  \ifthenelse{\equal{\@journal}{Toxins}}{www.mdpi.com/journal/toxins}{}%
  \ifthenelse{\equal{\@journal}{Vaccines}}{www.mdpi.com/journal/vaccines}{}%
  \ifthenelse{\equal{\@journal}{VeterinarySciences}}{www.mdpi.com/journal/vetsci}{}%
  \ifthenelse{\equal{\@journal}{Viruses}}{www.mdpi.com/journal/viruses}{}%
  \ifthenelse{\equal{\@journal}{Water}}{www.mdpi.com/journal/water}{}%
}

%% Define the length of the header on the front page
\newcommand{\logolength}{%
  \ifthenelse{\equal{\@journal}{Actuators}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{AdministrativeSciences}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Aerospace}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Agriculture}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Agronomy}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Algorithms}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Animals}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Antibiotics}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Antibodies}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Antioxidants}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{AppliedSciences}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Arts}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Atmosphere}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Atoms}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Axioms}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{BehavioralSciences}}{\begin{minipage}[t][30mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Bioengineering}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Biology}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Biomedicines}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Biomolecules}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Biosensors}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{BrainSciences}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Buildings}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Cancers}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Catalysts}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Cells}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Challenges}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Chemosensors}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Children}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Chromatography}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Climate}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Coatings}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Computation}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Computers}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Cosmetics}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Crystals}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{DentistryJournal}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Diagnostics}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Diseases}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Diversity}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Econometrics}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Economies}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Education}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Electronics}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Energies}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Entropy}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{EnvironmentalSciences}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Fibers}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Foods}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Forests}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{FutureInternet}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Galaxies}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Games}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Genes}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Geosciences}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Healthcare}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Humanities}}{\begin{minipage}[t][21mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Informatics}}{\begin{minipage}[t][21mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Information}}{\begin{minipage}[t][21mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Inorganics}}{\begin{minipage}[t][21mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Insects}}{\begin{minipage}[t][21mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Ijerph}}{\begin{minipage}[t][25mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Ijfs}}{\begin{minipage}[t][21mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Ijms}}{\begin{minipage}[t][21mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Ijgi}}{\begin{minipage}[t][21mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Jcdd}}{\begin{minipage}[t][30mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Jcm}}{\begin{minipage}[t][25mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Jdb}}{\begin{minipage}[t][30mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Jfb}}{\begin{minipage}[t][30mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Joi}}{\begin{minipage}[t][25mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Jlpea}}{\begin{minipage}[t][25mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Jmse}}{\begin{minipage}[t][30mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Jpm}}{\begin{minipage}[t][30mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Jrfm}}{\begin{minipage}[t][30mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Jsan}}{\begin{minipage}[t][21mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Land}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Laws}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Life}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Lubricants}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Machines}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{MarineDrugs}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Materials}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Mathematics}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{MedicalSciences}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Membranes}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Metals}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Metabolites}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Microarrays}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Micromachines}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Microorganisms}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Minerals}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}
  \ifthenelse{\equal{\@journal}{Molbank}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Molecules}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Nanomaterials}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Nutrients}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Optics}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Pathogens}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Pharmaceuticals}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Pharmaceutics}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Pharmacy}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Plants}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Polymers}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Processes}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Proteomes}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Publications}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Religions}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{RemoteSensing}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Resources}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Risks}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Robotics}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Sensors}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{SocialSciences}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Societies}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Sports}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Sustainability}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Symmetry}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Systems}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Technologies}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Toxics}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Toxins}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Vaccines}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{VeterinarySciences}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Viruses}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
  \ifthenelse{\equal{\@journal}{Water}}{\begin{minipage}[t][17mm][t]{0.97\textwidth}}{}%
}

%% Definde the header for the front page
\ifthenelse{\equal{\@arttype}{Supfile}}
{%
  \newcommand{\header}{%
    \thispagestyle{empty}%
  \begin{flushleft}%
    \vspace*{-50pt}%
  	\textbf{Supplementary file}%
  \end{flushleft}%
  }%
}%
{%
  \newcommand{\header}{%
\thispagestyle{empty}
\logolength
\begin{flushleft}
  \vspace{-36pt}
  \hspace{-5mm}{\ifthenelse{\equal{\@status}{submit}}{\normalfont Submitted to {\em \journalname}. Pages \thepage \ - \color{black}{\pageref{LastPage}}.
}{\normalfont {\em \journalname}\ {\bfseries {\@pubyear}}, {\em \@pubvolume},
\thepage -\@lastpage; doi:\@doinum}}
\end{flushleft}

%% Creates command that produces the logo on the first page of an accepted paper. For drafts the logo is made in white so that it cannot be seen, but leave the formatting unchanged
\vspace{-28pt}
\begin{flushright}
      {\fontsize{9}{9}\selectfont\bfseries{\colorbox{cyan}{\textcolor{white}{OPEN ACCESS}}}}\\[3pt]
% {\fontsize{10}{10}\selectfont\bfseries{\colorbox{cyan}{\textcolor{white}{~OPEN ACCESS~}}}}\\[8pt]
% This is the solution to be really comparable to the Word template
\ifthenelse{\equal{\@status}{submit}}{
        {{\logosize}{\logosize}\selectfont\bfseries\itshape \textcolor{white}{\logo}}\\
        \normalfont\bfseries \textcolor{white}{ISSN \issn }\\[-2pt]
        \normalfont \textcolor{white}{www.mdpi.com/journal/\logo}
        }
        {{\logosize\selectfont\bfseries\itshape \textcolor{logocolor}{\logo}}\\
        \normalfont\bfseries ISSN \issn\\[-2pt]
        \normalfont \web
        }
\end{flushright}
 \end{minipage}}
}

%% Maketitle
\renewcommand{\@maketitle}{%
\header

%%Title,author, address, abstract, keywords
\ifthenelse{\equal{\@arttype}{Supfile}}{}{
\begin{flushleft}%
 {%
 \fontsize{12}{12}\selectfont
  \vspace{0pt}
 \textit{\@arttype\\}
 \vspace{6pt}
 }%
  {%3
   \fontsize{18}{18}\selectfont
   \bfseries{\@Title}
   \par
  }%3
  \vspace{12pt}%
  {%4
   \bfseries{\@Author}
   \par
  }%4
  \vspace{12pt}%
{%6
\ifthenelse{\equal{\firstargument}{1}}{}{\hangafter=1\hangindent=0.7em\noindent}%
\@address
\par
}%6
\vspace{12pt}%
{%8
\ifthenelse{\equal{\@authornum}{authors}}{\hangafter=1\hangindent=0.7em\noindent
{\textbf{*}} \ifthenelse{\equal{\corresfirstargument}{2}}{Authors}{Author} to whom correspondence should be addressed; \@corres
\par
\vspace{12pt}}{}
}%8
\em
{%9
\ifthenelse{\equal{\@status}{submit}}{Version {\@ \today} submitted to {\em
\@journal}. Typeset by \LaTeX{} using class file mdpi.cls}{\@history}\par
}%9
\end{flushleft}}
}%End of maketitle

%% Command for typsetting abstract, kewyords and MSC, PACS
\newcommand{\abstractkeywords}{%
\vspace{12pt}%
\hrule

%% Specifies how the abstract should be typeset
{%10
\begingroup
 \vspace{6mm}
\leftskip1cm
\rightskip\leftskip
\parindent0em
  \noindent\textbf{Abstract:\space}\@abstract
  \vspace{3mm}
  \par
  \endgroup
}%10

%% Specifies how the keywords should be typeset and inserts a horizontal line afterwards if PACS and/or MSC are specified they are inserted after keywords, but before the line
{%11
\begingroup
 \vspace{3pt}
\leftskip1cm
\rightskip\leftskip
\parindent0em
     \textbf{Keywords:\space}\@keyword
 \vspace{12pt}
 \par
 \endgroup
}%11

%% Specifies how PACS should be typeset
{%12
\ifthenelse{\equal{\@pacs}{\@empty}}{}{%
\begingroup
\leftskip1cm
\rightskip\leftskip
\parindent0em
  \noindent\textbf{Classification: PACS\space} \@pacs
  \vspace{12pt}
  \par
  \endgroup}%
}%12

%% Specifies how PACS should be typeset
{%13
\ifthenelse{\equal{\@msc}{\@empty}}{}{%
\begingroup
\leftskip1cm
\rightskip\leftskip
\parindent0em
\noindent\textbf{Classification: MSC\space} \@msc
	\vspace{12pt}
   \par
  \endgroup}%%
}%13
\vspace{6pt}
\hrule
\vspace{27pt}
}%End of Abstract and Keywords
%
%
\ifthenelse{\equal{\@arttype}{Supfile}}%
{%
  \AtBeginDocument{%
    \maketitle%
    \thispagestyle{empty}%
    \let\maketitle\relax%
    \ifthenelse{\equal{\@status}{submit}}{\linenumbers}{}%
  }%
}%
{%
  \AtBeginDocument{%
    \maketitle
    \thispagestyle{empty}
    \let\maketitle\relax
    \ifthenelse{\equal{\@status}{submit}}{\linenumbers}{}
    \abstractkeywords
  }%
}%

%% Specifies how section headings should be typeset
\renewcommand{\thesection}{\arabic{section}}
\renewcommand{\section}{ %
  \@startsection{section}{1}{0pt}{12pt}{12pt}{\normalsize\bfseries}}

\renewcommand{\thesubsection}{\arabic{section}.\arabic{subsection}}
\renewcommand{\subsection}{ %
  \@startsection{subsection}{2}{0pt}{12pt}{12pt}{\normalsize\itshape}}

\renewcommand{\thesubsubsection}{\arabic{section}.\arabic{subsection}.\arabic{subsubsection}. }
\renewcommand{\subsubsection}{ %
  \@startsection{subsubsection}{3}{0pt}{12pt}{12pt}{\normalsize}}

%% Dot after number of section, but not in ref
\let\@@seccntformat\@seccntformat
\renewcommand*{\@seccntformat}[1]{%
  \expandafter\ifx\csname @seccntformat@#1\endcsname\relax
    \expandafter\@@seccntformat
  \else
    \expandafter
      \csname @seccntformat@#1\expandafter\endcsname
  \fi
    {#1}%
}

\newcommand*{\@seccntformat@subsubsection}[1]{%
  \csname the#1\endcsname
}

\renewcommand*{\@@seccntformat}[1]{%
  \csname the#1\endcsname.
}

%% Special section title style
\newcommand{\acknowledgements}[1]{%
    \par\vspace{12pt}\noindent\textbf{#1}%
    \par\vspace{12pt}}

%% Special section title style
\newcommand{\conflictofinterests}[1]{%
    \par\vspace{12pt}\noindent\textbf{#1}%
    \par\vspace{12pt}}    

%% Fix for two line titles in section
\def\section{\@ifstar\unnumberedsection\numberedsection}
\def\numberedsection{\@ifnextchar[%]
  \numberedsectionwithtwoarguments\numberedsectionwithoneargument}
\def\unnumberedsection{\@ifnextchar[%]
  \unnumberedsectionwithtwoarguments\unnumberedsectionwithoneargument}
\def\numberedsectionwithoneargument#1{\numberedsectionwithtwoarguments[#1]{#1}}
\def\unnumberedsectionwithoneargument#1{\unnumberedsectionwithtwoarguments[#1]{#1}}
\def\numberedsectionwithtwoarguments[#1]#2{%
  \ifhmode\par\fi
  \removelastskip
  \vskip 3ex\goodbreak
  \refstepcounter{section}%
  \noindent
  \leavevmode
  \begingroup
  \normalsize\bfseries
  \thesection.\
  #2 \vspace{12pt}
  \endgroup
  \addcontentsline{toc}{section}{%
    \protect\numberline{\thesection}%
    #1}%
  }
\def\unnumberedsectionwithtwoarguments[#1]#2{%
  \ifhmode\par\fi
  \removelastskip
  \vskip 3ex\goodbreak
%  \refstepcounter{section}%
  \noindent
  \leavevmode
  \begingroup
  \bfseries

%  \thesection\
  #2\quad
  \endgroup
  \addcontentsline{toc}{section}{%
%    \protect\numberline{\thesection}%
    #1}%
  }
  
%% Subsection fix for line indent
\def\subsection{\@ifstar\unnumberedsubsection\numberedsubsection}
\def\numberedsubsection{\@ifnextchar[%]
  \numberedsubsectionwithtwoarguments\numberedsubsectionwithoneargument}
\def\unnumberedsubsection{\@ifnextchar[%]
  \unnumberedsubsectionwithtwoarguments\unnumberedsubsectionwithoneargument}
\def\numberedsubsectionwithoneargument#1{\numberedsubsectionwithtwoarguments[#1]{#1}}
\def\unnumberedsubsectionwithoneargument#1{\unnumberedsubsectionwithtwoarguments[#1]{#1}}
\def\numberedsubsectionwithtwoarguments[#1]#2{%
  \ifhmode\par\fi
  \removelastskip
  \vskip 3ex\goodbreak
  \refstepcounter{subsection}%
  \noindent
  \leavevmode
  \begingroup
  \normalsize\itshape
  \thesubsection.\
  #2 \vspace{12pt}
  \endgroup
  \addcontentsline{toc}{subsection}{%
    \protect\numberline{\thesubsection}%
    #1}%
  }
\def\unnumberedsubsectionwithtwoarguments[#1]#2{%
  \ifhmode\par\fi
  \removelastskip
  \vskip 3ex\goodbreak
%  \refstepcounter{subsection}%
  \noindent
  \leavevmode
  \begingroup
  \bfseries
%  \thesubsection\
  #2\quad
  \endgroup
  \addcontentsline{toc}{subsection}{%
%    \protect\numberline{\the}%
    #1}%
  }

%\renewcommand*{\p@section}{\arabic{section}.}
%\renewcommand*{\p@subsection}{\p@section\thesection~}
%\renewcommand*{\p@subsubsection}{\p@subsection\thesubsection~}

%% Defines the appendix
\renewcommand{\appendix}{\par
  \setcounter{section}{0}%
  \setcounter{subsection}{0}%
  \setcounter{subsubsection}{0}%
  \gdef\thesection{\@Alph\c@section}%
  \gdef\thesubsection{\@Alph\c@section.\@arabic\c@subsection}%
  \gdef\theHsection{\@Alph\c@section.}%
  \gdef\theHsubsection{\@Alph\c@section.\@arabic\c@subsection}%
  \csname appendixmore\endcsname
}

%% Specifies how captions should be typeset
\renewcommand{\@makecaption}[2]{%
  \vskip\abovecaptionskip
  \sbox\@tempboxa{\normalsize{\bfseries #1}. #2}%
  \ifdim \wd\@tempboxa >\hsize
    \normalsize{\bfseries #1}. #2\par
  \else
    \global \@minipagefalse
    \hb@xt@\hsize{\hfil\box\@tempboxa\hfil}%
  \fi
  \vskip\belowcaptionskip}


\setlength{\oddsidemargin}{17.5mm}
\setlength{\voffset}{-1in}
\setlength{\hoffset}{-1in}
\setlength{\topmargin}{17.5mm}
\setlength{\headsep}{0mm}
\setlength{\textheight}{250mm}
\setlength{\textwidth}{175mm}
\setlength{\textfloatsep}{12pt}
\setlength{\abovedisplayskip}{0pt}
\setlength{\belowdisplayskip}{0pt}
\setlength{\parindent}{5mm}
\setlength{\abovecaptionskip}{12pt}
\setlength{\belowcaptionskip}{12pt}
\setlength{\itemsep}{0pt}
\setlength{\partopsep}{0pt}
\setlength{\parsep}{0pt}
\ifthenelse{\equal{\@arttype}{Supfile}}
{%
  \renewcommand{\thefigure}{S\arabic{figure}}%
  \renewcommand{\thetable}{S\arabic{table}}%
}{}%

%% Create headings for pages except the first
\newcommand{\heading}[1]{\gdef\@heading{#1}}
\setlength{\headheight}{8.5pt}
\setlength{\headsep}{25.5pt}

\def\ps@myheadings{%
    \def\@oddfoot{}
    \let\@evenfoot\@oddfoot
    \def\@evenhead{%
     
     {\ifthenelse{\equal{\@status}{submit}}{ \ifthenelse{\equal{\@arttype}{Supfile}}{}{Version {\@ \today} submitted to {\em \journalname}}%
        \hfil {\bfseries \thepage \ of \pageref{LastPage}}%
}{\ifthenelse{\equal{\@arttype}{Supfile}}{}{{\em \journalname} {\bfseries \@pubyear}, {\em \@pubvolume}}%
        \hfill {\bfseries \thepage}}%
        \vspace{50pt}}%
       }
    \let\@oddhead\@evenhead
    \let\@mkboth\@gobbletwo
    \let\sectionmark\@gobble
    \let\subsectionmark\@gobble
}

\pagestyle{myheadings}

%% Bibliography
\renewcommand\@biblabel[1]{#1. }

\def\thebibliography#1{\vspace{12pt}
\noindent{{\bfseries References}}\vspace{12pt}%
\list{{\arabic{enumi}}}{\def\makelabel##1{\hss{##1}}\topsep=0\p@\parsep=0\p@
\partopsep=0\p@\itemsep=0\p@
\labelsep=1ex
\itemindent=0\p@
\settowidth\labelwidth{\footnotesize[#1]}%
\leftmargin\labelwidth
\advance\leftmargin\labelsep
%\advance\leftmargin -\itemindent
\usecounter{enumi}}
%\def\newblock{\ }
%\sloppy\clubpenalty4000\widowpenalty4000
%\sfcode`\.=1000\relax
}
\let\endthebibliography=\endlist

%% Creates command that produces the copyright information
\newcommand{\cright}{%
\ifthenelse{\equal{\@arttype}{Supfile}}{}{%
\vspace{12pt}\noindent \copyright
\ifthenelse{\equal{\@status}{submit}}{\ {\@ \today} by the
\@authornum; submitted to {\em \journalname} for possible open
access publication under the terms and conditions of the
Creative Commons Attribution license
http://creativecommons.org/licenses/by/3.0/. }{\ {\@pubyear} by
the \@authornum; licensee MDPI, Basel, Switzerland. This article is an
open access article \linebreak distributed under the terms and conditions
of the Creative Commons Attribution license \linebreak
(http://creativecommons.org/licenses/by/3.0/).}}}

\endinput
