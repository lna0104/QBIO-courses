ALTER TABLE `Events` 
ADD CONSTRAINT `Chk_TicketPrice` 
CHECK (`TicketPrice` > 10.00  AND `TicketPrice` < 1000.00);

SELECT *
FROM Events;