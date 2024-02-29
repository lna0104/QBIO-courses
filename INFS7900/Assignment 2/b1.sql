DELETE FROM Medals
WHERE AthleteID = (
	SELECT AthleteID
    FROM Athletes
    WHERE AthleteName = 'Sarah Smith'
);