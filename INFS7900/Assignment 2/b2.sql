UPDATE Events
SET TicketPrice = TicketPrice*0.9
WHERE SportID NOT IN (
	SELECT SportID
    FROM Sports
    WHERE SportName = 'Basketball' OR SportName = 'Soccer'
);

UPDATE Events
SET Date = CURRENT_DATE() + 1;