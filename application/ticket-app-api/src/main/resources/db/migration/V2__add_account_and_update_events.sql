ALTER TABLE events DROP COLUMN deposit_url;

-- Create Account table
CREATE TABLE account (
                         account_id BIGINT AUTO_INCREMENT PRIMARY KEY,
                         deposit_url VARCHAR(255) NOT NULL,
                         account_number VARCHAR(50),
                         account_owner VARCHAR(100),
                         ticket_price INT,
                         `created_at` DATETIME(6) NULL DEFAULT NULL,
                         `modified_at` DATETIME(6) NULL DEFAULT NULL
);

-- Add account_id column to Events table
ALTER TABLE events ADD COLUMN account_id BIGINT;
