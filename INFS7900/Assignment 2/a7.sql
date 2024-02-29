SELECT C.CountryName
FROM Countries C
JOIN Athletes A ON C.CountryID = A.CountryID
JOIN Medals M ON A.AthleteID = M.AthleteID
WHERE M.MedalType = 'Gold'
GROUP BY C.CountryName
HAVING COUNT(M.MedalType) > 1;