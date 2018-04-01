USE PetRecords;
CREATE  OR REPLACE VIEW viewMedHistTimeline AS
SELECT 'med' AS 'group', 
    CONCAT(visit_id, '_', 'med') AS id,
	pet_name,
    vet_name,
    visit_date AS 'start',
    med_visit_summary AS content,
    visit_category AS category
FROM PetRecords.viewVisitsPetsVets
WHERE visit_category like 'medical%'
UNION
SELECT 'test' AS 'group', 
    CONCAT(test_id, '_', 'test') AS id,
	pet_name,
    vet_name,
    test_date_performed AS 'start',
    test_name AS content,
    test_category AS category
FROM PetRecords.viewTestsPetsVets
WHERE test_category like 'medical%';




