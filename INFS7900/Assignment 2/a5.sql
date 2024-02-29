SELECT CountryName
FROM Countries
WHERE CountryID IN (
	SELECT DISTINCT CountryID
	FROM Athletes
	WHERE Age > 30
); 