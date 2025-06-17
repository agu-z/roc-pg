
.PHONY: check-examples
check-examples: ## Check all Roc example files
	@echo "Checking all Roc examples..."
	@find examples -name "*.roc" -type f | while read -r file; do \
		echo "Checking $$file..."; \
		roc check "$$file" || exit 1; \
	done
	@echo "All examples passed roc check!"

.PHONY: ci-local
ci-local: ## Run GitHub Actions locally with podman
	@# Check if podman machine is running
	@if ! podman machine list --format "{{.Name}}\t{{.Running}}" | grep -q "true"; then \
		echo "Error: No podman machine is running. Please start it with 'podman machine start'"; \
		exit 1; \
	fi
	@# Get the podman socket path
	@PODMAN_SOCKET=$$(podman machine inspect --format '{{.ConnectionInfo.PodmanSocket.Path}}'); \
	if [ -z "$$PODMAN_SOCKET" ]; then \
		echo "Error: Could not determine podman socket path"; \
		exit 1; \
	fi; \
	export DOCKER_HOST="unix://$$PODMAN_SOCKET"; \
	exec act "$$@"
