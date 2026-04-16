
# Summarised contents

i_1 <- "based on initial Cooperation Programme scenarios and the assessed 'Consequences for the programme operations' in part 'ii. Analyses'"

i_2 <- "based on 'Consequences for the programme operations in part ii. Analyses
• Synthesize the shifts, if any, described under 'b) Consequences for the progrmame operations' of each chapter of part ii. Analyses with the focus on: 1) CoPr scenario; 2) CoPr assumptions and risks (RDM results framework); or 3) Major/severe project portfolio risks (SDC Projects risks).
• Describe how these shifts, if any, affect the programme implementation, and how the latter must be strategically or operationally adjusted and steered.
• If none of the above, leave empty
Optional: updates of interim assessments
"
i_3 <- "Describe how the described shifts under 'b) Consequences for the programme operations' 
of each chapter of part ii. Analyses shifts, if any, call for specific/additional
political dialogue and programme advocacy work
If none, leave empty
Optional: updates of interim assessments" 

if_none_empty <- "If none of the above, leave empty"

consequences_general <- "Please refer to the list of guiding questions at the end of the document."

conseq_all <- "
• Do the described shifts affect the identified: 1) CoPr scenario; 2) CoPr assumptions and risks (RDM results framework); or 3) major/severe portfolio risks (SDC Projects risks)
• Do the consequences of the described shifts affect the programme implementation, does the latter have to be strategically or operationally adjusted and steered, and if yes, how?
• Do the consequences of the described shifts call for specific/additional political dialogue
and programme advocacy work?"

conseq_1 <- "Do the described shifts affect the identified: 1) CoPr scenario; 2) CoPr assumptions and risks
(RDM results framework); or 3) major/severe portfolio risks (SDC Projects risks)?"
conseq_2 <- "Do the consequences of the described shifts affect the programme implementation, does
the latter have to be strategically or operationally adjusted and steered, and if yes, how?"
conseq_3 <- "Do the consequences of the described shifts call for specific/additional political dialogue
and programme advocacy work?"


# A Political System
## 1) International political context
analysis_pol <-"
• How are changes in regional/global relations (e.g. borders, trade, migration, shared resources) affecting political stability?
• Are there shifts in alliances, sanctions or external financing impacting the country's external posture?"
  
analysis_pol_1 <- "How are changes in regional/global relations (e.g. borders, trade, migration, shared resources)
affecting political stability?"
analysis_pol_2 <- "Are there shifts in alliances, sanctions or external financing impacting the country's external posture?"


## 2) Domestic Political Stability
analysis_delib <- "
• Are there any trends affecting the political system (e.g., democratic vs authoritarian regime type, constraints on executive power by legislative, judiciary or civil society/media, centralization  vs. decentralization)?
• Are there signs of changes in the political settlement (peaceful or violent), and how does this affect inclusion and stability?
• How has the governing coalition and civil-military relationship changed (e.g. civilian control of security, emergency rule, or unconstitutional extensions)?"

analysis_delib_1 <-
  "Are there any trends affecting the political system (e.g., democratic vs authoritarian regime type,
  constraints on executive power by legislative, judiciary or civil society/media, centralization
  vs. decentralization)?"
analysis_delib_2 <-
  "Are there signs of changes in the political settlement (peaceful or violent), and how does this affect
inclusion and stability"
analysis_delib_3 <-
  "How has the governing coalition and civil-military relationship changed (e.g. civilian control of security,
emergency rule, or unconstitutional extensions)?"

## 3) Civil and political rights, voice and media
analysis_civic <- "
• What are the recent trends in civil/political rights (expression, assembly, association, arbitrary detention, fair trial)? Is the legal/regulatory space for CSOs, media, academia changing (registration, funding, censorship, 'foreign agent' rules)?
• Which groups face new, heightened risks or unequal access, e.g. women, minorities, LGBTIQ+, IDPs/refugees, persons with disabilities?
• How did the control of key media/information channels and the prevalence of disinformation change?"

analysis_civic_1 <-
  "What are the recent trends in civil/political rights (expression, ¨assembly, association,
arbitrary detention, fair trial)? Is the legal/regulatory space for CSOs, media, academia changing
(registration, funding, censorship, 'foreign agent' rules)?"
analysis_civic_2 <-
  "Which groups face new, heightened risks or unequal access, e.g. women, minorities, LGBTIQ+,
  IDPs/refugees, persons with disabilities?"
analysis_civic_3 <-
  "How did the control of key media/information channels and the prevalence of disinformation change?"

## 4) Rule of law, independence of justice, division of power
analysis_rol <- "
• Have courts and prosecutors experienced changes in independence or insulation from political interference (e.g. appointments, budgets, case patterns, backlogs)?
• Have the effectiveness of checks and balances (legislative oversight, audits, due process) changed recently?
• Have there been notable shifts in the consistency and impartiality of law application, including emerging patterns of selective enforcement?"

analysis_rol_1 <- "Have courts and prosecutors experienced changes in independence or insulation from
political interference (e.g. appointments, budgets, case patterns, backlogs)?"
analysis_rol_2 <- "Have the effectiveness of checks and balances (legislative oversight, audits, due process)
changed recently?"
analysis_rol_3 <- "Have there been notable shifts in the consistency and impartiality of law application,
including emerging patterns of selective enforcement?"

# B) Development baselines

## 1) Economic prospects and systemic gaps
analysis_eco <- "
• What drives the latest trends in GNI per capita (e.g. exports, remittances, productivity)?
• What have been key macro-economic trends (e.g. growth, inflation, fiscal balance, debt sustainability)?
• Which sectors are driving growth and which ones are particularly vulnerable to external shocks?
• Which structural bottlenecks (e.g. business and investment climate, legal and regulatory environment, informal economy, corruption, finance and capital markets, energy, logistics, competition) most constrain productivity and private investment?"

analysis_eco_1 <- "What drives the latest trends in GNI per capita (e.g. exports, remittances, productivity)?"
analysis_eco_2 <- "What have been key macro-economic trends (e.g. growth, inflation, fiscal balance, debt
sustainability)?"
analysis_eco_3 <- "Which sectors are driving growth and which ones are particularly vulnerable to
external shocks?"
analysis_eco_4 <- "Which structural bottlenecks (e.g. business and investment climate, legal and regulatory
environment, informal economy, corruption, finance and capital markets, energy, logistics, competition)
most constrain productivity and private investment?"

## 2) Human capital, poverty and inequalities
analysis_hdi <- "
• Have there been any major shifts in poverty rates and inequality metrics (by region, gender, age, rural/urban, ethnicities, LGBTQIA+, migrants/IDPs or others), and how does it impact the distribution of wealth (Gini-coefficient)?
• What is the state of human capital (e.g. education, employability, professional skills), and are there major mismatches with labor market demand? What are the main challenges in developing human capital (e.g., quality, relevance, access, brain drain)?
• Have gender gaps in political and economic participation, access to services, or protection widened or narrowed recently?"

analysis_hdi_1 <- "Have there been any major shifts in poverty rates and inequality metrics (by region, gender,
 age, rural/urban, ethnicities, LGBTQIA+, migrants/IDPs or others), and how does it impact the distribution
of wealth (Gini-coefficient)?"
analysis_hdi_2 <- "What is the state of human capital (e.g. education, employability, professional skills),
and are there major mismatches with labor market demand? What are the main challenges in developing human
 capital (e.g., quality, relevance, access, brain drain)?"
analysis_hdi_3 <- "Have gender gaps in political and economic participation, access to services, or protection
 widened or narrowed recently?"


## 3) Climate & environment risks
analysis_env <- "
• What are emerging or intensifying environmental and natural hazards (heat, drought, floods, storms, sea-level rise, deforestation, water scarcity, pollution, biodiversity loss)?
• Which populations, sectors or regions are most exposed and vulnerable to these changing hazards?
• How credible and implementable are the country's climate/environment policies and plans (adaptation & disaster risk management, land-use, enforcement)?"

analysis_env_1 <- "What are emerging or intensifying environmental and natural hazards (heat, drought, floods,
 storms, sea-level rise, deforestation, water scarcity, pollution, biodiversity loss)?"
analysis_env_2 <- "Which populations, sectors or regions are most exposed and vulnerable to these changing
hazards?"
analysis_env_3 <- "How credible and implementable are the country's climate/environment policies and plans
(adaptation & disaster risk management, land-use, enforcement)?"

# C) Domestic Partner context
## 1) Operational space
operational_note <- "
Note: Analysis on security risks and access constraints to be done in 'KMZ Digital' and to be consulted prior to assessing the following guiding questions.
Disregard this section if there are no relevant security risks or access constraints."

operational_note_1 <- "Note: Analysis on security risks and access constraints to be done in 'KMZ Digital' and to be consulted prior to assessing the following guiding questions."
operational_note_2 <- "Disregard this section if there are no relevant security risks or access constraints."

analysis_ops <- "
• Are there new displacement/access constraints, and how is security forces' conduct impacting protection?
• Are there feasiable bypass options (e.g., negotiated access with local actors, alternative corridors/modes, timing windows, pre-positioning, remote/partner enabled delivery) to reach target populations/regions?
• Are the safeguarding, security risk management and contingency plan structures of programme/project partners adequate, and what does this imply for operational continuity of funded interventions and duty of care?"
  
analysis_ops_1 <- "Are there new displacement/access constraints, and how is security forces' conduct impacting
protection?"
analysis_ops_2 <- "Are there feasiable bypass options (e.g., negotiated access with local actors, alternative
corridors/modes, timing windows, pre-positioning, remote/partner enabled delivery) to reach target populations/
regions?"
analysis_ops_3 <- "Are the safeguarding, security risk management and contingency plan structures of programme/
project partners adequate, and what does this imply for operational continuity of funded interventions and duty
of care?"

## 2) Government effectiveness and control of corruption
analysis_gov <- "
• How are policy reforms and service delivery performing in the programme sectors, considering  available public resources/PFM, interministerial coordination, and staffing/skills - and which public services (e.g. education, health, water/sanitation) are most affected by quality/timeliness/coordination bottlenecks?
• What is the situation regarding merit-/competence-based staffing and clientelist placements (rent-seeking, control of resources), including at senior management levels, and how does this vary across sectors and governance levels (central vs. local administrations)?
• What do trends in integrity systems and enforcement reveal (procurement abuse, bribery, conflicts of interest, nepotism/favoritism; prosecutions, open data, asset declarations, whistleblower protections), and where are sector-specific corruption risks (licensing, SOEs, extractives, large infrastructure)  - in particular regarding exposure of programme pipelines?
• Are there key reform champions within government or agencies with the power and capabilities to advance policy reform in areas of concern, and has their presence or effectiveness changed recently?"
  
analysis_gov_1 <- "How are policy reforms and service delivery performing in the programme sectors, considering
 available public resources/PFM, interministerial coordination, and staffing/skills - and which public services
(e.g. education, health, water/sanitation) are most affected by quality/timeliness/coordination bottlenecks?"
analysis_gov_2 <- "What is the situation regarding merit-/competence-based staffing and clientelist placements
 (rent-seeking, control of resources), including at senior management levels, and how does this vary across
sectors and governance levels (central vs. local administrations)?"
analysis_gov_3 <- "What do trends in integrity systems and enforcement reveal (procurement abuse, bribery,
conflicts of interest, nepotism/favoritism; prosecutions, open data, asset declarations, whistleblower 
protections), and where are sector-specific corruption risks (licensing, SOEs, extractives, large infrastructure) 
- in particular regarding exposure of programme pipelines?"
analysis_gov_4 <- "Are there key reform champions within government or agencies with the power and capabilities
to advance policy reform in areas of concern, and has their presence or effectiveness changed recently?"

## 3) Role and relevance of official development assistance (ODA)
analysis_oda <- "
• How actively does government lead donor engagement (alignment with national plans, flexibility, conditionality), how effective are coordination platforms (sector working groups, join review, pooled funds), and where is potential lack of government commitment and ownership most acute?
• Who are the credible champions in programme sectors (institutions/individuals), what are their mandates and coalitions, and what political economy factors (veto players, patronage networks, media narratives) shape their room to maneuver?
• What is the volume, role, and trend of external financing (ODA grants/loans, climate funds, humanitarian aid, concessional financing), are the humanitarian fundingm gaps and who are the key donors/IFIs?
• Which support modalities (policy dialogue, TA, financing, convening power) and partnership opportunities (capacity and willingness for public co-financing, joint analytics, harmonized safeguards) can realistically amplify reform and long-term development momentum-and what risks (turnover, backlash) should be mitigated?"

analysis_oda_1 <- "How actively does government lead donor engagement (alignment with national plans, flexibility,
conditionality), how effective are coordination platforms (sector working groups, join review, pooled funds), 
and where is potential lack of government commitment and ownership most acute?"
analysis_oda_2 <- "Who are the credible champions in programme sectors (institutions/individuals), what are
their mandates and coalitions, and what political economy factors (veto players, patronage networks, 
media narratives) shape their room to maneuver?"
analysis_oda_3 <- "What is the volume, role, and trend of external financing (ODA grants/loans, climate funds,
 humanitarian aid, concessional financing), are the humanitarian fundingm gaps and who are the key donors/IFIs?"
analysis_oda_4 <- "Which support modalities (policy dialogue, TA, financing, convening power) and partnership
opportunities (capacity and willingness for public co-financing, joint analytics, harmonized safeguards) can 
realistically amplify reform and long-term development momentum-and what risks (turnover, backlash) should 
be mitigated?"

## 4) Non-state actors and private 
analysis_nsa <- "
• Which non-state actors like business organisations, corporates or cooperatives, civil society organisations (≠ donor-dependent not-for-profit implementers), community groups, faith-based organisations, academia, unions, etc. have reach, credibility, and technical capacity in target sectors/regions?
• How enabling is their operating environment (legal space, security, financing, data access), and what are the risks (politicization, low capacity, safeguarding concerns) and potential mitigations (due diligence, sustainable capacity development, strengthening of accountability)?
• What are Private Sector Engagement (PSE), Public-Private Partnerships (PPP) or blended finance opportunities (risk-sharing, guarantees, demand aggregation), and what execution risks (policy uncertainty, market distortion, undue subsidies, integrity) must be managed?"

analysis_nsa_1 <- "Which non-state actors like business organisations, corporates or cooperatives, civil society
organisations (≠ donor-dependent not-for-profit implementers), community groups, faith-based organisations, 
academia, unions, etc. have reach, credibility, and technical capacity in target sectors/regions?"
analysis_nsa_2 <- "How enabling is their operating environment (legal space, security, financing, data access),
 and what are the risks (politicization, low capacity, safeguarding concerns) and potential mitigations (due
diligence, sustainable capacity development, strengthening of accountability)?"
analysis_nsa_3 <- "What are Private Sector Engagement (PSE), Public-Private Partnerships (PPP) or blended finance
opportunities (risk-sharing, guarantees, demand aggregation), and what execution risks (policy uncertainty, 
market distortion, undue subsidies, integrity) must be managed?"







