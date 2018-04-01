USE PetRecords;
CREATE  OR REPLACE VIEW viewVaccineHistTimeline AS
SELECT pet_name,
    vaccine_name AS content,
    vaccine_date_given AS 'start',
    vaccine_date_expires AS 'end',
    vet_name,
    vaccine_current_flag AS current_flag,
    vaccine_certificate AS doc,
    CASE WHEN vaccine_current_flag = 'Y' THEN 'current'
    ELSE 'past'
    END AS className,
    DATEDIFF(vaccine_date_expires, CURDATE()) AS days_to_expiration
FROM PetRecords.viewVaccinesPetsVets
UNION 
SELECT pet_name,
    test_name AS content,
    test_date_performed AS 'start',
    test_date_expires AS 'end',
    vet_name,
    test_current_flag AS current_flag,
    test_result_doc AS doc,
    CASE WHEN test_current_flag = 'Y' THEN 'current'
    ELSE 'past'
    END AS className,
    DATEDIFF(test_date_expires, CURDATE()) AS days_to_expiration
FROM PetRecords.viewTestsPetsVets
WHERE test_current_flag IS NOT NULL;