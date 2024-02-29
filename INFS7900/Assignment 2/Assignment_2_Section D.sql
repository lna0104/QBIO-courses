--SECTION D - Critical Thinking
--Question 1
WITH d1 AS
    (
        SELECT 
            Week(Date) AS `Weeks`,
            COUNT(EventID) AS `NumbEvents`,
            COUNT(SportID) AS `NumbSports`
        FROM Events
        GROUP BY Weeks
    ),
    d2 AS 
    (
        SELECT 
            Week(Date) AS `Weeks`,
            COUNT(DISTINCT C.AthleteID) AS `NumbAltheles`
        FROM Events E
        JOIN Contestants C ON E.EventID = C.EventID
        GROUP BY Weeks
    )
SELECT d1.Weeks, d1.NumbEvents, d1.NumbSports, d2.NumbAltheles
FROM d1, d2
WHERE d1.Weeks = d2.Weeks
ORDER BY d1.Weeks

--Question 2
SELECT E.Date, COUNT(DISTINCT C.AthleteID) 
FROM Events E 
JOIN Contestants C ON E.EventID = C.EventID
GROUP BY E.Date
ORDER BY E.Date;
