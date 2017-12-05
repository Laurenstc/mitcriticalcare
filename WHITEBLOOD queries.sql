WITH t1 AS (
SELECT 
  diagnoses_icd.subject_id,
  diagnoses_icd.icd9_code, 
  diagnoses_icd.hadm_id
FROM 
  mimiciii.d_icd_diagnoses, 
  mimiciii.diagnoses_icd
WHERE diagnoses_icd.icd9_code IN ('28800', '28801', '28802', '28803', '28804', '28809', '28953', '7767')
GROUP BY diagnoses_icd.subject_id, diagnoses_icd.icd9_code, diagnoses_icd.hadm_id
limit 50000
)

SELECT t1.subject_id, t1.hadm_id, count(*) as count
FROM t1
GROUP BY t1.subject_id, t1.hadm_id
HAVING count(*) = 1
;


SOFA