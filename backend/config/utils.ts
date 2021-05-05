export class MissingConfigError extends Error {
  constructor(key: string) {
    super(`${key} is missing in the environment and is required`);
  }
}

export function env(key: string, required = true, defaultValue: string = null): string {
  const value = process.env[key];
  if (required && value === undefined) throw new MissingConfigError(key);
  return value || defaultValue;
}

export function isLocalEnv(): boolean {
  return (
    process.env.NODE_ENV === "development" || process.env.NODE_ENV === "test"
  );
}
