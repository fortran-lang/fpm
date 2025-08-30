# Custom Module Directory Example

This example demonstrates the use of a custom module directory in the `[install]` section of `fpm.toml`.

## Features

- Two simple Fortran modules: `greeting` and `math_utils`
- Custom module installation directory specified as `custom/modules`
- Shows how modules can be installed to a different location than headers

## Configuration

In `fpm.toml`:

```toml
[install]
library = true
module-dir = "custom/modules"
```

This configuration will install compiled `.mod` files to the `custom/modules` directory instead of the default `include` directory.

## Testing

To test this example:

```bash
cd example_packages/custom_module_dir
fpm build
fpm install --prefix /tmp/test_install
# Check that .mod files are in /tmp/test_install/custom/modules/
```