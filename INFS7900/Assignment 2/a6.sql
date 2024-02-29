SELECT AthleteName, Age
FROM Athletes
WHERE CountryID = (
	SELECT CountryID
    FROM Countries
    WHERE CountryName= 'Australia'
) AND Age <= ALL (
    SELECT Age
    FROM Athletes
    WHERE CountryID = (
	SELECT CountryID
    FROM Countries
    WHERE CountryName= 'Australia'
	)
);