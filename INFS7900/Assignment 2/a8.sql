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
