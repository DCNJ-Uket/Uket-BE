ALTER TABLE events DROP COLUMN deposit_url;

-- Create Account table
CREATE TABLE IF NOT EXISTS account (
                         account_id BIGINT AUTO_INCREMENT PRIMARY KEY,
                         deposit_url VARCHAR(255) NOT NULL,
                         account_number VARCHAR(255),
                         account_owner VARCHAR(255),
                         ticket_price INT,
                         `created_at` DATETIME(6) NULL DEFAULT NULL,
                         `modified_at` DATETIME(6) NULL DEFAULT NULL
);

-- Add account_id column to Events table
ALTER TABLE events ADD COLUMN account_id BIGINT;
ALTER TABLE events ADD COLUMN main_image_path VARCHAR(255);
