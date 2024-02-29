
--Section C - SQL DDL
--c1
CREATE TABLE Venues (
	VenueID INT NOT NULL, 
    VenueName VARCHAR(100) NOT NULL,
    VenueType ENUM ('Indoor','Outdoor','Covered') NOT NULL,
    CountryID INT NOT NULL,
    PRIMARY KEY (VenueID)
);
SELECT *
FROM Venues;

--c2
ALTER TABLE `Events` 
ADD CONSTRAINT `Chk_TicketPrice` 
CHECK (`TicketPrice` > 10.00  AND `TicketPrice` < 1000.00);

SELECT *
FROM Events;