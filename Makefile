# Run main.go
.PHONY: run
run:
	@go run main.go

# Tidy and format the code
.PHONY: tidy
tidy:
	@go fmt ./...
	@go mod tidy

.PHONY: test
test:
	@go test ./...
