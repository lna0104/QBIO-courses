SELECT C.CountryID, COUNT(M.MedalType)
FROM Medals M 
RIGHT JOIN Athletes A ON M.AthleteID = A.AthleteID
RIGHT JOIN Countries C ON A.CountryID = C.CountryID
GROUP BY C.CountryID;