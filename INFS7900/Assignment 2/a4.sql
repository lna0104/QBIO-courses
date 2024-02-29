SELECT MedalType, COUNT(MedalType)
FROM Medals 
WHERE AthleteID IN (
	SELECT AthleteID
    FROM Athletes
    WHERE CountryID = (
        SELECT CountryID
        FROM Countries
        WHERE CountryName= 'Australia'
    	)
)
GROUP BY MedalType;