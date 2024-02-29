-- Section B - SQL DML (UODATE, DELETE, INSERT)

--b1
DELETE FROM Medals
WHERE AthleteID = (
	SELECT AthleteID
    FROM Athletes
    WHERE AthleteName = 'Sarah Smith'
);

--b2
UPDATE Events
SET TicketPrice = TicketPrice*0.9
WHERE SportID NOT IN (
	SELECT SportsID
    FROM Sports
    WHERE SportName = 'Basketball' OR SportName = 'Soccer'
);

UPDATE Events
SET Date = CURRENT_DATE() + 1;

