
--t1 - icustay details
SELECT ie.subject_id, ie.hadm_id,  
  ie.icustay_id,
  adm.deathtime, 
  adm.ethnicity, 
  ie.intime, 
  ie.outtime, 
  pat.dob, 
  pat.dod	


-- patient level factors
, pat.gender

-- hospital level factors
, adm.admittime, adm.dischtime
, ROUND( (CAST(EXTRACT(epoch FROM adm.dischtime - adm.admittime)/(60*60*24) AS numeric)), 4) AS los_hospital
, ROUND( (CAST(EXTRACT(epoch FROM adm.admittime - pat.dob)/(60*60*24*365.242) AS numeric)), 4) AS age
, adm.ethnicity, adm.ADMISSION_TYPE
, adm.hospital_expire_flag
, DENSE_RANK() OVER (PARTITION BY adm.subject_id ORDER BY adm.admittime) AS hospstay_seq
, CASE
    WHEN DENSE_RANK() OVER (PARTITION BY adm.subject_id ORDER BY adm.admittime) = 1 THEN 'Y'
    ELSE 'N' END AS first_hosp_stay
-- icu level factors
, ie.intime, ie.outtime
, ROUND( (CAST(EXTRACT(epoch FROM ie.outtime - ie.intime)/(60*60*24) AS numeric)), 4) AS los_icu
, DENSE_RANK() OVER (PARTITION BY ie.hadm_id ORDER BY ie.intime) AS icustay_seq

-- first ICU stay *for the current hospitalization*
, CASE
    WHEN DENSE_RANK() OVER (PARTITION BY ie.hadm_id ORDER BY ie.intime) = 1 THEN 'Y'
    ELSE 'N' END AS first_icu_stay

FROM mimiciii.icustays ie
INNER JOIN mimiciii.admissions adm
    ON ie.hadm_id = adm.hadm_id
INNER JOIN mimiciii.patients pat
    ON ie.subject_id = pat.subject_id
WHERE adm.has_chartevents_data = 1 AND 
	(ROUND( (CAST(EXTRACT(epoch FROM adm.admittime - pat.dob)/(60*60*24*365.242) AS numeric)), 4) > 15) 
;