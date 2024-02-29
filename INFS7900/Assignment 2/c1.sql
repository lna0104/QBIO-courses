CREATE TABLE Venues (
	VenueID INT NOT NULL, 
    VenueName VARCHAR(100) NOT NULL,
    VenueType ENUM ('Indoor','Outdoor','Covered') NOT NULL,
    CountryID INT NOT NULL,
    PRIMARY KEY (VenueID)
);

SELECT *
FROM Venues;