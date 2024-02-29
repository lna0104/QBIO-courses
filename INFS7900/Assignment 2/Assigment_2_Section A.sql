-- Section A - SQL DML (SELECT)

-- a1
SELECT DISTINCT SportName
FROM Sports
ORDER BY SportName ASC;

--a2
SELECT SportID, COUNT(EventID)
FROM Events
WHERE Date >= '2023-07-01' AND Date <= '2023-07-31'
GROUP BY SportID;

--a3
SELECT C.CountryID, COUNT(M.MedalType)
FROM Medals M 
RIGHT JOIN Athletes A ON M.AthleteID = A.AthleteID
RIGHT JOIN Countries C ON A.CountryID = C.CountryID
GROUP BY C.CountryID;

--a4
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

--a5
SELECT CountryName
FROM Countries
WHERE CountryID IN (
	SELECT DISTINCT CountryID
	FROM Athletes
	WHERE Age > 30
); 

--a6
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

--a7
SELECT C.CountryName
FROM Countries C
JOIN Athletes A ON C.CountryID = A.CountryID
JOIN Medals M ON A.AthleteID = M.AthleteID
WHERE M.MedalType = 'Gold'
GROUP BY C.CountryName
HAVING COUNT(M.MedalType) > 1;

--a8

CREATE VIEW a8 AS
    SELECT SportID, Count(EventID) AS eventNumber
    FROM Events
    WHERE TicketPrice > 100
    GROUP BY SportID;
    
SELECT DISTINCT A.AthleteName
FROM Athletes A
JOIN Medals M ON A.AthleteID = M.AthleteID
JOIN Events E ON M.EventID = E.EventID
WHERE E.SportID IN (
	SELECT SportID
    FROM a8
    WHERE eventNumber >= 3
);





